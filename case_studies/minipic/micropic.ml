open Optitrust
open Prelude

(* let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true *)

(** Reproducing a subset of the PIC case study *)

let _ = Run.script_cpp (fun () ->

  let ctx = cFunBody "simulate_single_cell" in
  (* !! Resources.ensure_computed (); *)
  !! Resources.delete_annots [ctx];

  bigstep "make local copies of field and particles";
  let fv n = find_var_in_current_ast ~target:[ctx] n in
  let ft n = typ_var (find_var_in_current_ast_filter ~target:[ctx] (fun v -> v.name = n)) in
  !! Matrix.local_name (fv "fieldAtCorners") ~type_and_dims:(ft "vect", [trm_var (fv "nbCorners")]) ~into:"lFieldAtCorners" [ctx; cFor "idStep"];
  !! Matrix.local_name (fv "particles") ~type_and_dims:(ft "particle", [trm_var (fv "nbParticles")]) ~into:"lParticles" [ctx; cFor "idStep"];

  bigstep "inline helper functions and reveal record fields";
  !! Function.inline [ctx; multi cFun ["matrix_vect_mul"]];
  !! Function.inline [ctx; multi cFun ["vect_add"; "vect_mul"]];

  (* !! Variable.bind_multi ~const:true ~is_ptr:true ~dest:[tBefore; cVarDef "p"] "paddr" [nbMulti; ctx; cCellAccess ~base:[cVar "lParticles"] ()]; *)
  !!! Record.set_explicit [ctx; multi cArrayWrite ["particles"; "lParticles"]];
  !! Record.set_explicit [nbMulti; ctx; cWrite ~lhs:[Constr_depth (DepthAt 0); cAccesses ~base:[cOr (List.map (fun x -> [cVar x]) ["particles"; "lParticles"])] ~inner_accesses:false ~accesses:[cIndex (); cField ~regexp:true ~field:"\\(pos\\)\\|\\(speed\\)" ()] ()] ()];
  !! Record.set_explicit [ctx; multi cArrayWrite ["fieldAtCorners"; "lFieldAtCorners"]];
  !! Record.set_explicit [ctx; cWriteVar "fieldAtPos"];
  !! Record.to_variables [ctx; cVarDef "fieldAtPos"];
  !! Record.to_variables [ctx; cVarDefs ["pos2"; "speed2"; "accel"]];
  (* !! List.iter (fun f -> Record.inline f [cTypDef "particle"]) ["speed"; "pos"]; *)

  bigstep "scale field and particles";
  (*
  let p = trm_var (find_var_in_current_ast ~target:[ctx] "p") in
   (trm_struct_get p "charge")
   (trm_struct_get p "mass")
  *)
  let particleCharge = trm_var (find_var_in_current_ast ~target:[ctx] "particleCharge") in
  let particleMass = trm_var (find_var_in_current_ast ~target:[ctx] "particleMass") in
  let stepDuration = trm_var (find_var_in_current_ast ~target:[ctx] "stepDuration") in
  let scaleFieldFactor = trm_mul (trm_div particleCharge particleMass) (trm_mul stepDuration stepDuration) in
  let scaleField d = Accesses.scale ~factor:scaleFieldFactor [nbMulti; ctx; cVar ("fieldAtPos" ^ d)] in
  !! List.iter scaleField ["X"; "Y"; "Z"];
  !! Accesses.scale ~factor:scaleFieldFactor [nbMulti; ctx; cReadOrWrite ~addr:[cAccesses ~base:[cVar "lFieldAtCorners"] ~accesses:[cField ()] ()] ()];
  let scaleSpeed d = Accesses.scale_immut ~factor:stepDuration [nbMulti; ctx; cVarDef ("speed2" ^ d)] in
  !! List.iter scaleSpeed ["X"; "Y"; "Z"];
  !! Variable.inline [cVarDef "p"]; (* TODO: think about this *)

  !!! (); (* FIXME: why are things not found without reparse ? *)
  let stepDuration = trm_var (find_var_in_current_ast ~target:[ctx] "stepDuration") in

  !! Accesses.scale ~factor:stepDuration [nbMulti; ctx; cReadOrWrite ~addr:[cAccesses ~base:[cVar "lParticles"] ~accesses:[cField ~field:"speed" (); cField ()] ()] ()];

  bigstep "finish with style";
  !! Variable.inline [ctx; cVarDefs ["accelX"; "accelY"; "accelZ"; "pos2X"; "pos2Y"; "pos2Z"]];
  !!! Arith.(simpls_rec [expand; gather_rec]) [ctx];
  !! Function.use_infix_ops ~indepth:true [ctx];
  (* TODO: use_infix_ops *)
)
