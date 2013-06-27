(**
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation\; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Written and (c) by Pierre-Loic Garoche ONERA and Xavier Thirioux ENSEEIHT
   Contact <ploc@garoche.net> for comment & bug reports
*)


open Cil_types

(* Mapping between ACSL enumeration constant and prover tactics *)   
let strategy_assoc_table =
[
  "Intuition", "intuition";
  "Auto", "auto";
  "AffineEllipsoid", "affine_ellipsoid_strategy";
  "SProcedure", "s_procedure_strategy";
]

let grammar_extension_kwd = "PROOF_TACTIC"
let grammar_extension_id = 0
let grammar_extension_strategy_predicate = "use_strategy"

(*********************************************************************)
(*                 Plugin declaration                                *)
(*********************************************************************)
module Self = 
  Plugin.Register
    (struct
       let name = "local-tactic" 
       let shortname = "local-tactic"
       let help = "allow to declare local tactics in code annotation specs"
     end)

module Options =
struct
  module Enabled =
    Self.False(struct
		 let option_name = "-local-tactic"
		 let descr = "Local tactic in code specs" 
		 let help = "no help provided"
	       end)
end

(*********************************************************************)
(*                 Main functions                                    *)
(*********************************************************************)

let wp_compute_ip = 
  Dynamic.get 
    ~plugin:"Wp" 
    "wp_compute_ip" 
    (Datatype.func Property.ty Datatype.unit)

(* Registering tactic_term as the default strategy to be use *)
let set_tactic tactic_string =
  try
    let tactic_name = List.assoc tactic_string strategy_assoc_table in
    Dynamic.Parameter.String.set "-wp-tactic" tactic_name	
  with Not_found -> 
    Self.warning 
      "Tactic %s doesn't seem to be declared. Switching to default strategy." 
      tactic_string
 
let extract_tactic annot =
  match annot.annot_content with 
    | AStmtSpec (_, spec) -> (
      (* We iterate through grammar extensions. Only one of our kind is
	 allowed. In practice only the first is parsed. *)
      let tactic_term = 
	List.fold_left (fun res beh ->
	  match res, beh.b_extended with
	    | Some _, _ -> res
	    | None, [s, i, [p]]  
	      when grammar_extension_kwd = s 
	      && grammar_extension_id = i
		-> (
		  match p.ip_content with
		    | Papp (logic_info_, _, [term]) 
		      when
			logic_info_.l_var_info.lv_name 
			= grammar_extension_strategy_predicate ->
		    Some term
		  | Papp (logic_info_, _, _) 
		      when
			logic_info_.l_var_info.lv_name 
			= grammar_extension_strategy_predicate ->
		    Self.abort 
		      "Not a valid use of the local tactic extension, the predicate %s expect a single argument." 
		      grammar_extension_strategy_predicate
		  | _ -> Self.abort 
		    "Not a valid use of the local tactic extension, the expected predicate is %s." 
		    grammar_extension_strategy_predicate
	      )
	  (* either not a valid extension (not a single predicate as an
	     argument) or not our extension *)
	  | _ -> None 
      ) None spec.spec_behavior
      in
      match tactic_term with
	| Some (tt) -> ( 
	  match tt.term_node with
	    | TDataCons (ctor, []) -> 
	      (* Only allow enum const without args *)
	      Some ctor.ctor_name
	    | _ -> None
	)
	| _ -> None
    )
    | _ -> None (* Not an stmt spec *)

let extract_tactic = 
  Dynamic.register ~plugin:"local-tactic" "extract_tactic" ~journalize:false (Datatype.func Cil_datatype.Code_annotation.ty (Datatype.option Datatype.string)) extract_tactic
      
(* We only iterate through code annot (and not funspec) *)
let call_provers () =
  Annotations.iter_all_code_annot
    (
      fun stmt _ annot -> 
	let kf = Kernel_function.find_englobing_kf stmt in
	    let tactic_opt = extract_tactic annot in
	    match tactic_opt with
		Some tactic ->
		  let ips = Property.ip_of_code_annot kf stmt annot in
		  set_tactic tactic;
		  List.iter (wp_compute_ip) ips
	      | None -> () (* We only deal with our grammar extension annotations *)
    )
 
	
(*********************************************************************)
(*                 Syntax extension declaration                      *)
(*********************************************************************)

(* Typer for the grammar extension. Allow only a single predicate argument *)
let proof_tactic_typer ~typing_context ~loc bhv ps = 
  match ps with 
    | p::[] -> 
      bhv.b_extended 
      <- (grammar_extension_kwd, 
	  grammar_extension_id, 
	  [Logic_const.new_predicate 
	      (typing_context.Logic_typing.type_predicate 
		 (Logic_typing.Lenv.empty ()) p)
	  ]) ::bhv.b_extended 
    | _ -> 
      typing_context.Logic_typing.error
	loc
	"expecting a predicate after keyword %s" 
	grammar_extension_kwd

(* Register the grammar extension *)	
let () = Logic_typing.register_behavior_extension
  grammar_extension_kwd
  proof_tactic_typer
  

(*********************************************************************)
(*                  Plugin main function                             *)
(*********************************************************************)
   
let run () =
  if Options.Enabled.get () && Dynamic.is_plugin_present "Wp" then (
    (* Registering coq as the prover to use *)
    Dynamic.Parameter.StringSet.set 
      "-wp-proof" 
      (Datatype.String.Set.add "coq" Datatype.String.Set.empty);
    Ast.compute ();
    call_provers ();
    (* print_annotations (); *)
  (* check_dependencies (); *)
    (* default_parameters (); *)
	  
  )    

(** Extend the Frama-C entry point (the "main" of Frama-C). *)
let () = Db.Main.extend run

