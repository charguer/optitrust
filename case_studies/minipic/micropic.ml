open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

(** Reproducing a subset of the PIC case study *)

let _ = Run.script_cpp (fun () ->
  let ctx = cFunBody "simulate_single_cell" in
  let vect = typ_find_var "vect" [ctx] in
  let particle = typ_find_var "particle" [ctx] in
  let find_var n = trm_find_var n [ctx] in

  bigstep "make local copies of field and particles";
  !! Matrix.local_name_tile ~var:"fieldAtCorners"
    ~elem_ty:vect ~uninit_post:true ~mark_load:"loadField"
    ~local_var:"lFieldAtCorners" [ctx; cFor "idStep"];

  bigstep "inline helper functions and reveal record fields";
  !! Function.inline_multi [ctx; cFuns ["cornerInterpolationCoeff"; "matrix_vect_mul"; "vect_add"; "vect_mul"]];
  !! Record.split_fields ~typ:particle [tSpanSeq [ctx]];
  !! Record.split_fields ~typ:vect [tSpanSeq [ctx]];
  (* TODO: ~typs *)
  !! Record.to_variables [ctx; cVarDefs ["fieldAtPos"; "pos2"; "speed2"; "accel"]];

(* TODO:
  let fieldFactor = trm_mul (trm_mul (var "deltaT") (var "deltaT")) (trm_div (var "pCharge") (var "pMass")) in
  !! Variable.insert "fieldFactor" fieldFactor [tBefore; cVarDef "lFieldAtCorners"];
  !! Accesses.scale ~factor:(var "fieldFactor") [nbMulti; cVarRe "fieldAtPos[XYZ]"];
  !! Accesses.scale ~factor:(var "fieldFactor") [nbMulti; cVarRe "speed2[XYZ]"];
  !! Accesses.scale ~factor:(var "deltaT") [nbMulti; sExprRe "particles\\.speed\\.[xyz]"];
  !! Accesses.scale_immut ~factor:(var "deltaT") [nbMulti; cVarRe "speed2[XYZ]"];
*)
  bigstep "scale field and particles";
  let deltaT = find_var "deltaT" in
  let fieldFactor = trm_mul (trm_mul deltaT deltaT) (trm_div (find_var "pCharge") (find_var "pMass")) in
  (* Variable.def + inline on scope *)
  let scaleFieldAtPos d = Accesses.scale_var ~factor:fieldFactor [nbMulti; ctx; cVarDef ("fieldAtPos" ^ d)] in
  !! List.iter scaleFieldAtPos ["X"; "Y"; "Z"];
  let scaleSpeed2 d = Accesses.scale_immut ~factor:deltaT [nbMulti; ctx; cVarDef ("speed2" ^ d)] in
  !! List.iter scaleSpeed2 ["X"; "Y"; "Z"];
  let scaleFieldAtCorners field =
    let address_pattern = Trm.(struct_access (array_access (find_var "lFieldAtCorners") (pattern_var "i")) field) in
    Accesses.scale ~factor:fieldFactor ~address_pattern ~uninit_post:true [ctx; tSpan [tBefore; cMark "loadField"] [tAfter; cFor "idStep"]]
  in
  !! List.iter scaleFieldAtCorners ["x"; "y"; "z"];
  let scaleParticles field =
    let address_pattern = Trm.(struct_access (struct_access (array_access (find_var "particles") (pattern_var "i")) "speed") field) in
    Accesses.scale ~factor:deltaT ~address_pattern [ctx; tSpanAround [cFor "idStep"]];
  in
  !! List.iter scaleParticles ["x"; "y"; "z"];

  bigstep "arithmetic simplifications";
  (* TODO: hoist coeffs *)
  !! Loop.fusion_targets [cFor ~body:[cMul ~lhs:[cVar "particles"] ()] "i1"]; (* marks pre/post + List.iter *)
  !! Loop.fusion_targets [cFor ~body:[cDiv ~lhs:[cVar "particles"] ()] "i1"];
  (* INLINE fieldFactor *)
  !! Variable.inline [ctx; cVarDefs ["accelX"; "accelY"; "accelZ"; "pos2X"; "pos2Y"; "pos2Z"]];
  (* accel product dims *)
  !!! Arith.(simpls_rec [expand; gather_rec]) [ctx];

  bigstep "final polish";
  !! Function.use_infix_ops ~indepth:true [ctx]; (* TODO: check *)
  !! Cleanup.std (); (* cleanup += 1 --> ++ *)

  (* TODO:
    - cleanup script
    - local name tile: get elem_ty from program / resources
    - allow writing C code for constructing factors, need to parse and put in correct context with local ids
    - insert var for scaleFieldFactor
    - bind pointer to particles cell?
    - put 'coeffs' array on stack?
    - FIXME: reparse triggers access normalization
  *)
)
