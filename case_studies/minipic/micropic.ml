open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true


(** Reproducing a subset of the PIC case study *)

let _ = Run.script_cpp (fun () ->
  Convention.default_naming_policy := Naming_underscore;

  let ctx = cFunBody "simulate_single_cell" in
  let find_var n = trm_find_var n [ctx] in
  let vect = typ_find_var "vect" [ctx] in
  let particle = typ_find_var "particle" [ctx] in
  let dims = ["x"; "y"; "z"] in

  bigstep "make local copy of field";
  !! Matrix.local_name_tile ~var:"fieldAtCorners"
    ~elem_ty:vect ~uninit_post:true ~mark_load:"loadField"
    ~local_var:"lFieldAtCorners" [ctx; cFor "idStep"];

  bigstep "inline helper functions and reveal record fields";
  !! Function.inline_multi [ctx; cFuns ["cornerInterpolationCoeff"; "matrix_vect_mul"; "vect_add"; "vect_mul"]];
  !! Record.split_fields ~typs:[particle; vect] [tSpanSeq [ctx]];
  !! Record.to_variables [ctx; cVarDefs ["fieldAtPos"; "pos2"; "speed2"; "accel"]];

  bigstep "scale field and particles";
  let deltaT = find_var "deltaT" in
  !! Variable.insert ~name:"fieldFactor" ~value:(trm_mul (trm_mul deltaT deltaT) (trm_div (find_var "pCharge") (find_var "pMass"))) [ctx; tBefore; cVarDef "lFieldAtCorners"];
  (* tFirst *)
  let scaleFieldAtPos d = Accesses.scale_var ~factor:(find_var "fieldFactor") [nbMulti; ctx; cVarDef ("fieldAtPos_" ^ d)] in
  !! List.iter scaleFieldAtPos dims;
  let scaleSpeed2 d = Accesses.scale_immut ~factor:deltaT [nbMulti; ctx; cVarDef ("speed2_" ^ d)] in
  !! List.iter scaleSpeed2 dims;
  let scaleFieldAtCorners d = (* lFieldAtCorners[?i].(x|y|z) *)
    let address_pattern = Trm.(struct_access (array_access (find_var "lFieldAtCorners") (pattern_var "i")) d) in
    Accesses.scale ~factor:(find_var "fieldFactor") ~address_pattern ~uninit_post:true [ctx; tSpan [tBefore; cMark "loadField"] [tAfter; cFor "idStep"]]in
  !! List.iter scaleFieldAtCorners dims;
  let scaleParticles d = (* particles[?i].speed.(x|y|z)] *)
    let address_pattern = Trm.(struct_access (struct_access (array_access (find_var "particles") (pattern_var "i")) "speed") d) in
    Accesses.scale ~factor:deltaT ~address_pattern ~mark_preprocess:"partsPrep" ~mark_postprocess:"partsPostp" [ctx; tSpanAround [cFor "idStep"]]; in
  !! List.iter scaleParticles dims;
  !! List.iter Loop.fusion_targets [[cMark "partsPrep"]; [cMark "partsPostp"]];

  bigstep "inline variables and simplify arithmetic";
  !! Variable.unfold ~at:[cFor "idStep"] [cVarDef "fieldFactor"];
  !! Variable.inline [ctx; cVarDefs (Tools.cart_prod (^) ["accel_"; "pos2_"] dims)];
  !!! Arith.(simpls_rec [expand; gather_rec]) [ctx];

  bigstep "final polish";
  !! Loop.hoist_alloc ~indep:["idStep"; "idPart"] ~dest:[tBefore; cFor "idStep"] [cVarDef "coeffs"];
  !! Cleanup.std ();
)
