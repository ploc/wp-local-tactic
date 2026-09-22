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
module Lang = Wp.Lang
module Conditions = Wp.Conditions

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

(* Named recipes: [\local-tactic::rocq_strategy Name: "ltac", "ltac2", ...;] *)
let recipes : (string, string) Hashtbl.t = Hashtbl.create 7

(* extension id -> (recipe name, script) -- kept for pretty-printing *)
let strategy_decls : (int, string * string) Hashtbl.t = Hashtbl.create 7

(* one element of a (possibly multi-line) script: a literal Rocq chunk -- a
   genuine tactic, or a bare "(* ... *)" comment for readability -- or a
   reference to a named [rocq_strategy] recipe. Segments are concatenated
   with "\n" once resolved. *)
type segment =
  | Lit of string
  | Ref of string       (* \by(Name) -- reference to a named recipe *)

type script_body = segment list

type script = {
  label : string option;  (* optional property-name filter *)
  body : script_body;
}

(* [rocq_script] extension id -> parsed script *)
let scripts : (int, script) Hashtbl.t = Hashtbl.create 7

(* [rocq_proof <lemma>: ...] extension id -> (lemma name, script) *)
let proofs : (int, string * script_body) Hashtbl.t = Hashtbl.create 7

(* [rocq_alias <lemma>: "Name";] extension id -> (lemma name, Rocq alias) *)
let aliases : (string, string) Hashtbl.t = Hashtbl.create 7
let alias_decls : (int, string * string) Hashtbl.t = Hashtbl.create 7
(* reverse map: alias -> lemma, to catch two lemmas claiming the same alias *)
let alias_owner : (string, string) Hashtbl.t = Hashtbl.create 7

let fresh_id = let c = ref 0 in fun () -> incr c ; !c

(* -------------------------------------------------------------------------- *)
(* --- ACSL extension typers                                             --- *)
(* -------------------------------------------------------------------------- *)

let as_string : Logic_ptree.lexpr -> string option =
  let open Logic_ptree in
  function
  | { lexpr_node = PLconstant (StringConstant s); _ } -> Some s
  | _ -> None

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_ident_char c = is_ident_start c || (c >= '0' && c <= '9') || c = '\''

let valid_coq_ident s =
  s <> "" && is_ident_start s.[0] &&
  String.for_all is_ident_char s

(* [\local-tactic::rocq_strategy Name: "line1", "line2", ...;] -- each
   string is one line of the recipe, joined with "\n" *)
let rocq_strategy_typer (ctxt : Logic_typing.typing_context) loc lexprs =
  let open Logic_ptree in
  match lexprs with
  | { lexpr_node = PLnamed (name, first); _ } :: rest ->
    let line e =
      match as_string e with
      | Some s -> s
      | None ->
        ctxt.error loc
          "rocq_strategy '%s' expects Rocq script string literals" name
    in
    let s = String.concat "\n" (List.map line (first :: rest)) in
    Hashtbl.replace recipes name s ;
    let id = fresh_id () in
    Hashtbl.replace strategy_decls id (name, s) ;
    Ext_id id
  | _ ->
    ctxt.error loc
      "expecting 'Name: \"<rocq script>\", ...' after \\local-tactic::rocq_strategy"

(* parse one segment of a script body: a string literal (a tactic, or a bare
   "(* ... *)" comment), or [\by(Name)] *)
let parse_segment (ctxt : Logic_typing.typing_context) ~kw loc lexpr =
  let open Logic_ptree in
  match lexpr with
  | { lexpr_node = PLapp ("\\by", [], [ { lexpr_node = PLvar n; _ } ]); _ } ->
    Ref n
  | { lexpr_node = PLapp ("\\by", [], [ arg ]); _ } ->
    (match as_string arg with
     | Some n -> Ref n
     | None -> ctxt.error loc "\\by(...) expects a recipe name")
  | single ->
    (match as_string single with
     | Some s -> Lit s
     | None ->
       ctxt.error loc
         "%s expects a Rocq script string literal or \\by(Name)" kw)

(* a script body is one or more segments -- string literals and/or
   [\by(Name)] -- concatenated with "\n" once resolved. Writing several
   short strings, one per line, lets a script carry Rocq "(* ... *)"
   comments between tactics without cramming everything on one line. *)
let parse_body (ctxt : Logic_typing.typing_context) ~kw loc lexprs =
  match lexprs with
  | [] ->
    ctxt.error loc
      "%s expects at least one Rocq script string literal or \\by(Name)" kw
  | l -> List.map (parse_segment ctxt ~kw loc) l

(* [rocq_script [label:] ( "ltac", ... | \by(Name) );] -- on a contract *)
let rocq_script_typer (ctxt : Logic_typing.typing_context) loc lexprs =
  let open Logic_ptree in
  let label, rest =
    match lexprs with
    | { lexpr_node = PLnamed (l, p); _ } :: tl -> Some l, p :: tl
    | l -> None, l
  in
  let body = parse_body ctxt ~kw:"rocq_script" loc rest in
  let id = fresh_id () in
  Hashtbl.replace scripts id { label ; body } ;
  Ext_id id

(* [rocq_proof <lemma>: ( "ltac", ... | \by(Name) );] -- global, targets a
   lemma *)
let rocq_proof_typer (ctxt : Logic_typing.typing_context) loc lexprs =
  let open Logic_ptree in
  match lexprs with
  | { lexpr_node = PLnamed (lemma, p); _ } :: tl ->
    let body = parse_body ctxt ~kw:"rocq_proof" loc (p :: tl) in
    let id = fresh_id () in
    Hashtbl.replace proofs id (lemma, body) ;
    Ext_id id
  | _ ->
    ctxt.error loc
      "expecting '<lemma>: ( \"<rocq script>\", ... | \\by(Name) )' after \
       rocq_proof"

(* [rocq_alias <lemma>: "Name";] -- global; declares a short Rocq name for
   the axiom WP emits for [lemma] ('Q_<lemma>', see [Wp.Lang.lemma_id])
   wherever that axiom shows up in a generated .v *)
let rocq_alias_typer (ctxt : Logic_typing.typing_context) loc lexprs =
  let open Logic_ptree in
  match lexprs with
  | [ { lexpr_node = PLnamed (lemma, body); _ } ] ->
    (match as_string body with
     | Some alias ->
       if not (valid_coq_ident alias) then
         ctxt.error loc
           "rocq_alias '%s': '%s' is not a valid Rocq identifier" lemma alias ;
       (match Hashtbl.find_opt alias_owner alias with
        | Some other when other <> lemma ->
          ctxt.error loc
            "rocq_alias '%s': alias '%s' is already used for lemma '%s'"
            lemma alias other
        | _ -> ()) ;
       Hashtbl.replace aliases lemma alias ;
       Hashtbl.replace alias_owner alias lemma ;
       let id = fresh_id () in
       Hashtbl.replace alias_decls id (lemma, alias) ;
       Ext_id id
     | None ->
       ctxt.error loc "rocq_alias '%s' expects a Rocq identifier string" lemma)
  | _ ->
    ctxt.error loc "expecting '<lemma>: \"<RocqName>\"' after rocq_alias"

let pp_segment fmt = function
  | Lit s -> Format.fprintf fmt "%S" s
  | Ref n -> Format.fprintf fmt "\\by(%s)" n

let pp_body fmt segs =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
    pp_segment fmt segs

let pp_script_kind fmt = function
  | Ext_id id ->
    (match Hashtbl.find_opt scripts id with
     | Some { label ; body } ->
       (match label with Some l -> Format.fprintf fmt "%s: " l | None -> ()) ;
       pp_body fmt body
     | None -> ())
  | _ -> ()

let pp_strategy_kind fmt = function
  | Ext_id id ->
    (match Hashtbl.find_opt strategy_decls id with
     | Some (name, body) -> Format.fprintf fmt "%s: %S" name body
     | None -> ())
  | _ -> ()

let pp_proof_kind fmt = function
  | Ext_id id ->
    (match Hashtbl.find_opt proofs id with
     | Some (lemma, body) -> Format.fprintf fmt "%s: %a" lemma pp_body body
     | None -> ())
  | _ -> ()

let pp_alias_kind fmt = function
  | Ext_id id ->
    (match Hashtbl.find_opt alias_decls id with
     | Some (lemma, alias) -> Format.fprintf fmt "%s: %S" lemma alias
     | None -> ())
  | _ -> ()

let () =
  Acsl_extension.register_global ~plugin:"local-tactic" "rocq_strategy"
    rocq_strategy_typer
    ~printer:(fun _ fmt k -> pp_strategy_kind fmt k)
    false ;
  Acsl_extension.register_global ~plugin:"local-tactic" "rocq_proof"
    rocq_proof_typer
    ~printer:(fun _ fmt k -> pp_proof_kind fmt k)
    false ;
  Acsl_extension.register_global ~plugin:"local-tactic" "rocq_alias"
    rocq_alias_typer
    ~printer:(fun _ fmt k -> pp_alias_kind fmt k)
    false ;
  Acsl_extension.register_behavior ~plugin:"local-tactic" "rocq_script"
    rocq_script_typer
    ~printer:(fun _ fmt k -> pp_script_kind fmt k)
    false ;
  (* [rocq_loop_script] shares [rocq_script]'s typer/registry -- a loop
     invariant is not a behavior clause, so it needs its own keyword
     (Acsl_extension keys registrations by (plugin, name) alone, regardless
     of grammar position; the same name can't be registered twice) but the
     same parsing and storage applies verbatim. *)
  Acsl_extension.register_code_annot_next_loop ~plugin:"local-tactic"
    "rocq_loop_script"
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

(* true if [id] occurs in [hay] as a standalone identifier (not as part of
   a longer one) *)
let contains_ident hay id =
  let n = String.length id and h = String.length hay in
  if n = 0 then false else
    let rec loop i =
      if i + n > h then false
      else
        let before_ok = i = 0 || not (is_ident_char hay.[i - 1]) in
        let after_ok = i + n = h || not (is_ident_char hay.[i + n]) in
        if before_ok && after_ok && String.sub hay i n = id then true
        else loop (i + 1)
    in
    loop 0

(* [rocq_alias <lemma>: "Name"] declarations whose target axiom
   ('Q_<lemma>', see [Wp.Lang.lemma_id]) is actually present in [content] --
   as a "Notation Name := Q_<lemma>." line per hit, marking [seen] so unused
   aliases can be reported once the whole run is done *)
let alias_lines_for ~(seen : (string, unit) Hashtbl.t) content =
  Hashtbl.fold (fun lemma alias acc ->
      let coq_id = Lang.lemma_id lemma in
      if contains_ident content coq_id then begin
        Hashtbl.replace seen lemma () ;
        Printf.sprintf "Notation %s := %s." alias coq_id :: acc
      end else acc)
    aliases []

(* -------------------------------------------------------------------------- *)
(* --- Readable hypothesis names (best-effort)                           --- *)
(* -------------------------------------------------------------------------- *)

(* Why3's own pass-1 skeleton is a single "intros <tok> ... <tok>." line
   between "Proof." and "Qed."/"Admitted.". Extract its tokens, or [None] if
   the skeleton isn't in that shape (e.g. a more complex proof structure). *)
let extract_intros_tokens content =
  try
    let goal_pos = before_substring ~start:0 goal_marker content in
    let proof_end = after_substring ~start:goal_pos "\nProof." content in
    let stop kw = try Some (before_substring ~start:proof_end kw content)
      with Not_found -> None in
    match stop "\nQed.", stop "\nAdmitted." with
    | None, None -> None
    | Some i, None | None, Some i | Some i, Some _ ->
      let skeleton = String.trim (String.sub content proof_end (i - proof_end)) in
      let len = String.length skeleton in
      if len < 8 || String.sub skeleton 0 6 <> "intros"
         || skeleton.[len - 1] <> '.'
      then None
      else
        let body = String.sub skeleton 6 (len - 6 - 1) in
        let toks =
          String.split_on_char ' ' body
          |> List.concat_map (String.split_on_char '\n')
          |> List.concat_map (String.split_on_char '\t')
          |> List.filter (fun t -> t <> "")
        in
        if toks <> [] && List.for_all valid_coq_ident toks then Some toks
        else None
  with Not_found -> None

(* the printed "Theorem wp_goal : <this> ." statement, i.e. everything from
   the goal marker up to (excluding) "Proof." *)
let extract_statement content =
  try
    let goal_pos = before_substring ~start:0 goal_marker content in
    let proof_start = before_substring ~start:goal_pos "\nProof." content in
    Some (String.sub content goal_pos (proof_start - goal_pos))
  with Not_found -> None

(* number of top-level (paren-depth 0) "->" in a goal statement: each one is
   exactly one flattened hypothesis, regardless of foralls/lets/parens *)
let count_top_level_arrows text =
  let n = String.length text in
  let rec loop i depth acc =
    if i >= n then acc
    else match text.[i] with
      | '(' -> loop (i + 1) (depth + 1) acc
      | ')' -> loop (i + 1) (depth - 1) acc
      | '-' when depth = 0 && i + 1 < n && text.[i + 1] = '>' ->
        loop (i + 2) depth (acc + 1)
      | _ -> loop (i + 1) depth acc
  in
  loop 0 0 0

(* the ACSL 'requires' name a hypothesis step comes from, if any *)
let requires_name (p : Property.t) =
  match p with
  | Property.IPPredicate { ip_kind = Property.PKRequires _; _ } ->
    (match Property.get_names p with n :: _ -> Some n | [] -> None)
  | _ -> None

(* Best-effort "intros" line using the 'requires' clause names instead of
   Why3's generic h1/h2/... A hypothesis step is renamed when its dependency
   set (Conditions.step.deps) points to exactly one named 'requires'.

   WP's sequent (Conditions.sequent, via Wpo.compute) is not a literal,
   position-for-position preimage of the printed forall/-> chain: 'State'
   steps are memory-model bookkeeping with no printed counterpart, and
   'Type' steps (is_sintN/is_uintN range facts) are printed *after* the
   'Have' ones rather than in their internal order. Both are corrected for
   below. When a goal shares a computed value across hypotheses via a
   printed "let", Why3 can also introduce extra range hypotheses that never
   appear in Conditions.sequent at all -- undetectable from the WP side, so
   as a hard safety net the reconstructed hypothesis count is cross-checked
   against an independent count of top-level "->" in the printed statement;
   any mismatch bails out to no renaming at all instead of a wrong one. *)
let build_auto_intros (wpo : Wpo.t) (raw : string) : string option =
  match extract_intros_tokens raw, extract_statement raw with
  | None, _ | _, None -> None
  | Some tokens, Some statement ->
    let sequent = snd (Wpo.compute wpo) in
    let steps = Conditions.list (fst sequent) in
    let blocking =
      List.exists
        (fun (s : Conditions.step) ->
           match s.Conditions.condition with
           | Conditions.Branch _ | Conditions.Either _ | Conditions.Probe _ -> true
           | _ -> false)
        steps
    in
    (* 'Have'-like steps first (their relative order), 'Type' steps after
       (their relative order) -- matches Why3's printed convention; 'State'
       steps carry no hypothesis at all and are dropped. *)
    let group (s : Conditions.step) =
      match s.Conditions.condition with
      | Conditions.Have _ | Conditions.When _
      | Conditions.Core _ | Conditions.Init _ -> Some 0
      | Conditions.Type _ -> Some 1
      | Conditions.State _ | Conditions.Branch _
      | Conditions.Either _ | Conditions.Probe _ -> None
    in
    let hyp_steps =
      List.stable_sort (fun a b -> compare (group a) (group b))
        (List.filter (fun s -> group s <> None) steps)
    in
    let n_steps = List.length hyp_steps and n_tokens = List.length tokens in
    let n_arrows = count_top_level_arrows statement in
    if blocking || n_steps = 0 || n_steps > n_tokens || n_arrows <> n_steps
    then None
    else
      let n_lead = n_tokens - n_steps in
      let arr = Array.of_list tokens in
      let leading = Array.to_list (Array.sub arr 0 n_lead) in
      let trailing = Array.to_list (Array.sub arr n_lead n_steps) in
      let used = Hashtbl.create 7 in
      List.iter (fun t -> Hashtbl.replace used t ()) leading ;
      let renamed = ref [] in
      let trailing' =
        List.map2
          (fun tok (step : Conditions.step) ->
             match List.find_map requires_name step.Conditions.deps with
             | Some name when valid_coq_ident name && not (Hashtbl.mem used name) ->
               Hashtbl.replace used name () ;
               renamed := (tok, name) :: !renamed ;
               name
             | _ -> tok)
          trailing hyp_steps
      in
      if !renamed = [] then None
      else
        let comment =
          Printf.sprintf "(* local-tactic intros: %s *)"
            (String.concat ", "
               (List.rev_map (fun (tok, name) -> Printf.sprintf "%s -> %s" tok name)
                  !renamed))
        in
        let intros_line =
          Printf.sprintf "intros %s." (String.concat " " (leading @ trailing'))
        in
        Some (comment ^ "\n" ^ intros_line)

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

let resolve_segment = function
  | Lit s -> Some s
  | Ref n ->
    (match Hashtbl.find_opt recipes n with
     | Some s -> Some s
     | None -> Self.warning "unknown rocq_strategy recipe '%s' (skipped)" n ; None)

(* joins a script's segments with "\n"; [None] if any [\by(Name)] segment
   fails to resolve (already warned about) *)
let resolve_body (segs : script_body) : string option =
  let rec go acc = function
    | [] -> Some (String.concat "\n" (List.rev acc))
    | seg :: tl ->
      (match resolve_segment seg with
       | Some s -> go (s :: acc) tl
       | None -> None)
  in
  go [] segs

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
  (* statement contracts, and [rocq_loop_script] on a loop invariant *)
  Annotations.iter_all_code_annot (fun stmt _ ca ->
      match ca.annot_content with
      | AStmtSpec (_, spec) ->
        let kf = Kernel_function.find_englobing_kf stmt in
        scan_spec kf (Kstmt stmt) spec
      | AExtended (_, true, { ext_name = "rocq_loop_script"; ext_kind = Ext_id id; _ }) ->
        (match Hashtbl.find_opt scripts id with
         | None -> ()
         | Some sc ->
           (match resolve_body sc.body with
            | None -> ()
            | Some ltac ->
              let kf = Kernel_function.find_englobing_kf stmt in
              List.iter
                (fun ca2 ->
                   match ca2.annot_content with
                   | AInvariant (_, true (* normal loop invariant *), _) ->
                     let ip = Property.ip_of_code_annot_single kf stmt ca2 in
                     let keep =
                       match sc.label with
                       | None -> true
                       | Some l -> List.mem l (Property.get_names ip)
                     in
                     if keep then out := (ip, ltac) :: !out
                   | _ -> ())
                (Annotations.code_annot stmt)))
      | _ -> ()) ;
  (* function contracts *)
  Globals.Functions.iter (fun kf ->
      match Annotations.funspec kf with
      | spec -> scan_spec kf Kglobal spec
      | exception Not_found -> ()) ;
  (* lemmas, via [rocq_proof <lemma>: ...] global annotations *)
  if Hashtbl.length proofs > 0 then begin
    let wanted = Hashtbl.create 7 in
    Hashtbl.iter (fun _ (lemma, body) -> Hashtbl.replace wanted lemma (body, ref false)) proofs ;
    (* [iter_global] does not descend into axiomatics/modules -- do it here *)
    let rec visit g =
      match g with
      | Daxiomatic (_, gs, _, _) | Dmodule (_, gs, _, _, _) -> List.iter visit gs
      | Dlemma (name, _, _, _, _, _) ->
        (match Hashtbl.find_opt wanted name with
         | None -> ()
         | Some (body, seen) ->
           seen := true ;
           (match resolve_body body with
            | None -> ()
            | Some ltac ->
              List.iter
                (fun ip -> match ip with
                   | Property.IPLemma _ -> out := (ip, ltac) :: !out
                   | _ -> ())
                (Property.ip_of_global_annotation g)))
      | _ -> ()
    in
    Annotations.iter_global (fun _ g -> visit g) ;
    Hashtbl.iter
      (fun lemma (_, seen) ->
         if not !seen then
           Self.warning "rocq_proof: no lemma named '%s' (skipped)" lemma)
      wanted
  end ;
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

        (* splice imports + rocq_alias notations + user script into each
           generated file *)
        let alias_seen : (string, unit) Hashtbl.t = Hashtbl.create 7 in
        List.iter (fun (ip, ltac) ->
            List.iter (fun (wpo : Wpo.t) ->
                let f = script_file wpo in
                if Sys.file_exists f then begin
                  let raw = read_file f in
                  let alias_imports = alias_lines_for ~seen:alias_seen raw in
                  (* don't prepend an auto 'intros' in front of a script that
                     already starts with one of its own *)
                  let trimmed = String.trim ltac in
                  let has_own_intros =
                    String.length trimmed >= 6
                    && String.sub trimmed 0 6 = "intros"
                  in
                  let ltac =
                    if has_own_intros then ltac
                    else
                      match build_auto_intros wpo raw with
                      | Some block -> block ^ "\n" ^ ltac
                      | None -> ltac
                  in
                  match splice ~imports:(imports @ alias_imports) ~ltac raw with
                  | Some spliced -> write_file f spliced
                  | None ->
                    Self.warning
                      "%a: unexpected layout in %s, script not spliced"
                      Property.pretty ip f
                end else if Wpo.is_fully_valid wpo then
                  (* this particular goal (e.g. a loop invariant's
                     'established' half, next to a harder 'preserved' one)
                     was already closed by Qed without needing an
                     interactive prover at all -- nothing to splice *)
                  ()
                else
                  Self.warning "%a: expected Rocq file %s was not generated"
                    Property.pretty ip f)
              (Wpo.goals_of_property ip))
          targets ;
        Hashtbl.iter
          (fun lemma alias ->
             if not (Hashtbl.mem alias_seen lemma) then
               Self.warning
                 "rocq_alias: '%s' (lemma '%s') was not applied -- its axiom \
                  did not appear in any file generated by this run"
                 alias lemma)
          aliases ;

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
