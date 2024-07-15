open Optitrust
open Prelude

(* let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true *)

(** Reproducing a subset of the PIC case study *)

let _ = Run.script_cpp (fun () ->

  let ctx = cFunBody "simulate_single_cell" in
  (* !! Resources.ensure_computed (); *)
  !! Resources.delete_annots [ctx];

  bigstep "inline helper functions and reveal record fields";
  !! Function.inline [ctx; multi cFun ["matrix_vect_mul"]];
  !! Function.inline [ctx; multi cFun ["vect_add"; "vect_mul"]];

  (* !! Variable.bind_multi ~const:true ~is_ptr:true ~dest:[tBefore; cVarDef "p"] "paddr" [nbMulti; ctx; cCellAccess ~base:[cVar "particles"] ()]; *)
  !! Record.set_explicit [nbMulti; ctx; cFor "idStep"; cArrayWrite "particles"];
  !! Record.set_explicit [ctx; cWriteVar "fieldAtPos"];
  !! Record.to_variables [ctx; cVarDef "fieldAtPos"];
  !! Record.to_variables [ctx; multi cVarDef ["pos2"; "speed2"; "accel"]];
  (* !! List.iter (fun f -> Record.inline f [cTypDef "particle"]) ["speed"; "pos"]; *)

  let fv n = find_var_in_current_ast ~target:[ctx] n in
  let ft n = typ_var (find_var_in_current_ast_filter ~target:[ctx] (fun v -> v.name = n)) in
  bigstep "scale field and particles locally";
  !! Matrix.local_name (fv "fieldAtCorners") ~type_and_dims:(ft "vect", [trm_var (fv "nbCorners")]) ~into:"lFieldAtCorners" [ctx; cFor "idStep"];
  !! Matrix.local_name (fv "particles") ~type_and_dims:(ft "particle", [trm_var (fv "nbParticles")]) ~into:"lParticles" [ctx; cFor "idStep"];
  !! Accesses.scale ~factor:(trm_float 2.0) [nbMulti; ctx; cVar "fieldAtPosX"]; (* add constants, check / 0 *)

  (* use_infix_ops *)
)
