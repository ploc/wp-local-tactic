(**
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation; either version 2.1 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   Copyright (c) 2026 Pierre-Loic Garoche, ENAC
   Implemented with the help of Claude (Anthropic).
   Contact <ploc@garoche.net> for comment & bug reports
*)

open Cil_types

(* WP exposes its API as a wrapped library ([frama-c-wp.core], module [Wp]). *)
module VC = Wp.VC
module Wpo = Wp.Wpo
module Why3Provers = Wp.Why3Provers
module Wp_parameters = Wp.Wp_parameters

(* -------------------------------------------------------------------------- *)
(* --- Plugin declaration                                                --- *)
(* -------------------------------------------------------------------------- *)

module Self =
  Plugin.Register
    (struct
      let name = "local-tactic"
      let shortname = "local-tactic"
      let help =
        "Integrated Rocq (Coq) proofs from ACSL annotations: an inline proof \
         script attached to a contract is spliced into the proof obligation's \
         generated .v file, checked with coqc, and its verdict reported \
         through WP."
    end)

module Enabled =
  Self.False
    (struct
      let option_name = "-local-tactic"
      let help =
        "run the integrated Rocq proof pipeline on contracts carrying a \
         'rocq_script' clause (off by default)"
    end)

module RocqImports =
  Self.String_list
    (struct
      let option_name = "-local-tactic-rocq-import"
      let arg_name = "lib.mod,..."
      let help =
        "extra Rocq libraries to load in every generated .v file. Each entry \
         'x.y' becomes 'From x Require Import y.'; an entry without a dot \
         becomes 'Require Import x.'"
    end)

module Prover =
  Self.String
    (struct
      let option_name = "-local-tactic-prover"
      let arg_name = "name"
      let default = "Coq"
      let help =
        "WP prover used to check the generated .v files (default: 'Coq'). Set \
         it to a distinct prover name registered through '-wp-why3-extra-config' \
         to run coqc from another opam switch (e.g. one that provides the \
         'validsdp' tactic)."
    end)

(* -------------------------------------------------------------------------- *)
(* --- Registries populated at typing time                               --- *)
(* -------------------------------------------------------------------------- *)

(* Named recipes: [\local-tactic::rocq_strategy Name: "ltac";] *)
let recipes : (string, string) Hashtbl.t = Hashtbl.create 7

(* extension id -> (recipe name, script) -- kept for pretty-printing *)
let strategy_decls : (int, string * string) Hashtbl.t = Hashtbl.create 7

type script_body =
  | Inline of string    (* literal Ltac script *)
  | Ref of string       (* \by(Name) -- reference to a named recipe *)

type script = {
  label : string option;  (* optional property-name filter *)
  body : script_body;
}

(* [rocq_script] extension id -> parsed script *)
let scripts : (int, script) Hashtbl.t = Hashtbl.create 7

let fresh_id = let c = ref 0 in fun () -> incr c ; !c

(* -------------------------------------------------------------------------- *)
(* --- ACSL extension typers                                             --- *)
(* -------------------------------------------------------------------------- *)

let as_string : Logic_ptree.lexpr -> string option =
  let open Logic_ptree in
  function
  | { lexpr_node = PLconstant (StringConstant s); _ } -> Some s
  | _ -> None

(* [\local-tactic::rocq_strategy Name: "ltac script";] *)
let rocq_strategy_typer (ctxt : Logic_typing.typing_context) loc lexprs =
  let open Logic_ptree in
  match lexprs with
  | [ { lexpr_node = PLnamed (name, body); _ } ] ->
    (match as_string body with
     | Some s ->
       Hashtbl.replace recipes name s ;
       let id = fresh_id () in
       Hashtbl.replace strategy_decls id (name, s) ;
       Ext_id id
     | None ->
       ctxt.error loc
         "rocq_strategy '%s' expects a Rocq script string literal" name)
  | _ ->
    ctxt.error loc
      "expecting 'Name: \"<rocq script>\"' after \\local-tactic::rocq_strategy"

(* [rocq_script [label:] ( "ltac" | \by(Name) );] *)
let rocq_script_typer (ctxt : Logic_typing.typing_context) loc lexprs =
  let open Logic_ptree in
  let label, rest =
    match lexprs with
    | { lexpr_node = PLnamed (l, p); _ } :: tl -> Some l, p :: tl
    | l -> None, l
  in
  let body =
    match rest with
    | [ { lexpr_node =
            PLapp ("\\by", [], [ { lexpr_node = PLvar n; _ } ]); _ } ] -> Ref n
    | [ { lexpr_node =
            PLapp ("\\by", [], [ arg ]); _ } ] ->
      (match as_string arg with
       | Some n -> Ref n
       | None -> ctxt.error loc "\\by(...) expects a recipe name")
    | [ single ] ->
      (match as_string single with
       | Some s -> Inline s
       | None ->
         ctxt.error loc
           "rocq_script expects a Rocq script string literal or \\by(Name)")
    | _ ->
      ctxt.error loc
        "rocq_script expects a single Rocq script string literal or \\by(Name)"
  in
  let id = fresh_id () in
  Hashtbl.replace scripts id { label ; body } ;
  Ext_id id

let pp_script_kind fmt = function
  | Ext_id id ->
    (match Hashtbl.find_opt scripts id with
     | Some { label ; body } ->
       (match label with Some l -> Format.fprintf fmt "%s: " l | None -> ()) ;
       (match body with
        | Inline s -> Format.fprintf fmt "%S" s
        | Ref n -> Format.fprintf fmt "\\by(%s)" n)
     | None -> ())
  | _ -> ()

let pp_strategy_kind fmt = function
  | Ext_id id ->
    (match Hashtbl.find_opt strategy_decls id with
     | Some (name, body) -> Format.fprintf fmt "%s: %S" name body
     | None -> ())
  | _ -> ()

let () =
  Acsl_extension.register_global ~plugin:"local-tactic" "rocq_strategy"
    rocq_strategy_typer
    ~printer:(fun _ fmt k -> pp_strategy_kind fmt k)
    false ;
  Acsl_extension.register_behavior ~plugin:"local-tactic" "rocq_script"
    rocq_script_typer
    ~printer:(fun _ fmt k -> pp_script_kind fmt k)
    false

(* -------------------------------------------------------------------------- *)
(* --- Rocq file splicing                                                --- *)
(* -------------------------------------------------------------------------- *)

let goal_marker = "(* Why3 goal *)"

let import_lines () =
  RocqImports.get ()
  |> List.concat_map (String.split_on_char ',')
  |> List.filter_map (fun e ->
      let e = String.trim e in
      if e = "" then None
      else match String.rindex_opt e '.' with
        | Some i when i > 0 && i < String.length e - 1 ->
          Some (Printf.sprintf "From %s Require Import %s."
                  (String.sub e 0 i)
                  (String.sub e (i + 1) (String.length e - i - 1)))
        | _ -> Some (Printf.sprintf "Require Import %s." e))

(* Find the byte index just after the first occurrence of [needle] at/after
   [start], or raise Not_found. *)
let after_substring ~start needle hay =
  let n = String.length needle and h = String.length hay in
  let rec loop i =
    if i + n > h then raise Not_found
    else if String.sub hay i n = needle then i + n
    else loop (i + 1)
  in
  loop start

let before_substring ~start needle hay =
  let n = String.length needle and h = String.length hay in
  let rec loop i =
    if i + n > h then raise Not_found
    else if String.sub hay i n = needle then i
    else loop (i + 1)
  in
  loop start

let sub_from s i = String.sub s i (String.length s - i)

(* Splice imports + user script into a Why3/Coq generated file. Returns the
   new content, or [None] if the expected markers are missing.

   Layout produced by Why3's Coq driver:

     ...preamble...
     (* Why3 goal *)
     Theorem wp_goal : <statement> .
     Proof.
     <skeleton>
     Qed.            (* or Admitted., or nothing *)
*)
let splice ~imports ~ltac (content : string) : string option =
  try
    let goal_pos = before_substring ~start:0 goal_marker content in
    (* keep everything up to and including "Proof." after the goal marker *)
    let proof_end = after_substring ~start:goal_pos "\nProof." content in
    (* drop the skeleton and its terminator, if any *)
    let rest_start =
      let stop kw = try Some (after_substring ~start:proof_end kw content)
        with Not_found -> None in
      match stop "\nQed.", stop "\nAdmitted." with
      | Some i, _ | _, Some i -> i
      | None, None -> String.length content
    in
    let imports_block =
      if imports = [] then ""
      else "\n" ^ String.concat "\n" imports ^ "\n"
    in
    Some (String.concat ""
            [ String.sub content 0 goal_pos ;         (* preamble *)
              imports_block ;
              String.sub content goal_pos (proof_end - goal_pos) ; (* ..Proof. *)
              "\n" ; ltac ; "\nQed." ;
              sub_from content rest_start ])          (* trailing, if any *)
  with Not_found -> None

(* -------------------------------------------------------------------------- *)
(* --- Proof pipeline                                                    --- *)
(* -------------------------------------------------------------------------- *)

let coq_prover () =
  let name = Prover.get () in
  match Why3Provers.lookup ~fallback:true name with
  | Some p -> p
  | None ->
    Self.abort
      "prover '%s' not found in the Why3 configuration.@ \
       For the default 'Coq', run 'why3 config detect' with Rocq installed;@ \
       for another name, register it with '-wp-why3-extra-config'."
      name

let interactive_dir () =
  Wp_parameters.get_session_dir ~force:true "interactive"

let script_file wpo =
  let dir = interactive_dir () in
  Filepath.to_string (Filepath.concat dir (wpo.Wpo.po_sid ^ ".v"))

let read_file f =
  let ic = open_in_bin f in
  Fun.protect ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let write_file f s =
  let oc = open_out_bin f in
  Fun.protect ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc s)

let resolve_body = function
  | Inline s -> Some s
  | Ref n ->
    (match Hashtbl.find_opt recipes n with
     | Some s -> Some s
     | None -> Self.warning "unknown rocq_strategy recipe '%s' (skipped)" n ; None)

(* collect (property, ltac) pairs from every [rocq_script] clause, both on
   statement contracts and on function contracts *)
let collect_targets () =
  let out = ref [] in
  let handle kf ki sc bhv =
    match resolve_body sc.body with
    | None -> ()
    | Some ltac ->
      let ips = Property.ip_ensures_of_behavior kf ki bhv in
      let keep ip =
        (match ip with
         | Property.IPPredicate { ip_kind = Property.PKEnsures (_, Normal); _ } ->
           true
         | _ -> false)
        && (match sc.label with
            | None -> true
            | Some l -> List.mem l (Property.get_names ip))
      in
      List.iter (fun ip -> if keep ip then out := (ip, ltac) :: !out) ips
  in
  let scan_spec kf ki spec =
    List.iter (fun bhv ->
        List.iter (fun (ext : acsl_extension) ->
            if ext.ext_name = "rocq_script" then
              match ext.ext_kind with
              | Ext_id id ->
                (match Hashtbl.find_opt scripts id with
                 | Some sc -> handle kf ki sc bhv
                 | None -> ())
              | _ -> ())
          bhv.b_extended)
      spec.spec_behavior
  in
  (* statement contracts *)
  Annotations.iter_all_code_annot (fun stmt _ ca ->
      match ca.annot_content with
      | AStmtSpec (_, spec) ->
        let kf = Kernel_function.find_englobing_kf stmt in
        scan_spec kf (Kstmt stmt) spec
      | _ -> ()) ;
  (* function contracts *)
  Globals.Functions.iter (fun kf ->
      match Annotations.funspec kf with
      | spec -> scan_spec kf Kglobal spec
      | exception Not_found -> ()) ;
  !out

let run_pipeline () =
  (* fail early and clearly if Rocq is not available to Why3 *)
  ignore (coq_prover () : Why3Provers.t) ;
  let imports = import_lines () in
  let targets = collect_targets () in
  if targets = [] then
    Self.result "no 'rocq_script' clause found (nothing to do)"
  else begin
    (* remember and force WP prover configuration *)
    let saved_provers = Wp_parameters.Provers.get () in
    let saved_inter = Wp_parameters.Interactive.get () in
    let saved_status = Wp_parameters.StatusTrue.get () in
    let restore () =
      Wp_parameters.Provers.set saved_provers ;
      Wp_parameters.Interactive.set saved_inter ;
      Wp_parameters.StatusTrue.set saved_status
    in
    (* force a fresh Coq proof obligation for each target, discarding any
       verdict a prior '-wp' run may have produced with other provers. *)
    let regen ip =
      VC.remove ip ;
      let g = VC.generate_ip ip in
      Self.debug ~level:1 "%a: %d goal(s) generated" Property.pretty ip
        (Bag.length g) ;
      g
    in
    Fun.protect ~finally:restore (fun () ->
        Wp_parameters.Provers.set [ Prover.get () ] ;
        (* regenerate obligations even for properties a prior '-wp' run has
           already marked 'Valid' with another prover. *)
        Wp_parameters.StatusTrue.set true ;

        (* pass 1: let WP generate the .v script(s) from scratch *)
        Wp_parameters.Interactive.set "update" ;
        let pass1 = List.map (fun (ip, _) -> regen ip) targets in
        List.iter (fun (ip, _) ->
            List.iter (fun (wpo : Wpo.t) ->
                let f = script_file wpo in
                if Sys.file_exists f
                then (try Sys.remove f with Sys_error _ -> ()))
              (Wpo.goals_of_property ip))
          targets ;
        VC.command (Bag.ulist pass1) ;

        (* splice imports + user script into each generated file *)
        List.iter (fun (ip, ltac) ->
            List.iter (fun (wpo : Wpo.t) ->
                let f = script_file wpo in
                if Sys.file_exists f then
                  match splice ~imports ~ltac (read_file f) with
                  | Some spliced -> write_file f spliced
                  | None ->
                    Self.warning
                      "%a: unexpected layout in %s, script not spliced"
                      Property.pretty ip f
                else
                  Self.warning "%a: expected Rocq file %s was not generated"
                    Property.pretty ip f)
              (Wpo.goals_of_property ip))
          targets ;

        (* pass 2: recompile the spliced file(s) as-is *)
        Wp_parameters.Interactive.set "batch" ;
        VC.command (Bag.ulist (List.map (fun (ip, _) -> regen ip) targets)))
  end

let run () =
  if Enabled.get () then begin
    if not (Plugin.is_present "wp") then
      Self.abort "the WP plug-in is required by -local-tactic"
    else begin
      Ast.compute () ;
      run_pipeline ()
    end
  end

let () = Boot.Main.extend run
