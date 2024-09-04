open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

(** Reproducing a subset of the PIC case study *)

let _ = Run.script_cpp (fun () ->
  let ctx = cFunBody "simulate_single_cell" in
  let _var n = trm_var (find_var_in_current_ast ~target:[ctx] n) in
  let typv n = find_var_in_current_ast_filter ~target:[ctx] (fun v -> v.name = n) in
  let typ n = typ_var (typv n) in

  bigstep "make local copies of field and particles";
  !! Matrix.local_name_tile ~var:"fieldAtCorners"
    ~elem_ty:(typ "vect") ~uninit_post:true
    ~local_var:"lFieldAtCorners" [ctx; cFor "idStep"];
  !! Matrix.local_name_tile ~var:"particles"
    ~elem_ty:(typ "particle")
    ~local_var:"lParticles" [ctx; cFor "idStep"];

  bigstep "inline helper functions and reveal record fields";
  !! Function.inline ~recurse:true [ctx; multi cFun ["matrix_vect_mul"; "vect_add"; "vect_mul"]];
  !! Record.split_fields ~typ:(typv "particle") [ctx; tSpan [tAfter; cVarDef "lFieldAtCorners"] [tLast]];
  !! ignore (raise Pattern.Failed);
  (* !! Record.set_explicit [ctx; multi cArrayWrite ["particles"; "lParticles"]];
  !! Record.set_explicit [nbMulti; ctx; cWrite ~lhs:[Constr_depth (DepthAt 0); cAccesses ~base:[cOr (List.map (fun x -> [cVar x]) ["particles"; "lParticles"])] ~inner_accesses:false ~accesses:[cIndex (); cField ~regexp:true ~field:"\\(pos\\)\\|\\(speed\\)" ()] ()] ()];
  !! Record.set_explicit [ctx; multi cArrayWrite ["fieldAtCorners"; "lFieldAtCorners"]];
  !! Variable.bind ~const:true "fieldAtPosTmp" [cWriteVar "fieldAtPos"; dArg 1];
  !! Record.set_explicit [ctx; cWriteVar "fieldAtPos"]; *)
  !! Record.to_variables [ctx; cVarDefs ["fieldAtPos"; "pos2"; "speed2"; "accel"]];

(* TODO:
  !! Record.split_fields [nbMulti; ctx; cWrite ~typ:[multi cEq [vect; particle]] []];
  // ;
  !! Record.to_variables [ctx; cVarDefs ["fieldAtPos"; "pos2"; "speed2"; "accel"]];

  let fieldFactor = trm_mul (trm_mul (var "deltaT") (var "deltaT")) (trm_div (var "pCharge") (var "pMass")) in
  !! Variable.insert "fieldFactor" fieldFactor [tBefore; cVarDef "lFieldAtCorners"];
  !! Accesses.scale ~factor:(var "fieldFactor") [nbMulti; cVarRe "fieldAtPos[XYZ]"];
  !! Accesses.scale ~factor:(var "fieldFactor") [nbMulti; cVarRe "speed2[XYZ]"];
  !! Accesses.scale ~factor:(var "deltaT") [nbMulti; sExprRe "particles\\.speed\\.[xyz]"];
  !! Accesses.scale_immut ~factor:(var "deltaT") [nbMulti; cVarRe "speed2[XYZ]"];

  !! Variable.inline [ctx; cVarDefs ["accelX"; "accelY"; "accelZ"; "pos2X"; "pos2Y"; "pos2Z"]];
  !!! Arith.(simpls_rec [expand; gather_rec]) [ctx];
  !! Function.use_infix_ops ~indepth:true [ctx];
  !! Cleanup.std ();
*)

  (* CHECK *)
  bigstep "scale field and particles";
  let pCharge = trm_var (find_var_in_current_ast ~target:[ctx] "pCharge") in
  let pMass = trm_var (find_var_in_current_ast ~target:[ctx] "pMass") in
  let deltaT = trm_var (find_var_in_current_ast ~target:[ctx] "deltaT") in
  let scaleFieldFactor = trm_mul (trm_div pCharge pMass) (trm_mul deltaT deltaT) in
  let scaleField d = Accesses.scale ~factor:scaleFieldFactor [nbMulti; ctx; cVar ("fieldAtPos" ^ d)] in
  !! List.iter scaleField ["X"; "Y"; "Z"];
  !! Accesses.scale ~factor:scaleFieldFactor [nbMulti; ctx; cReadOrWrite ~addr:[cAccesses ~base:[cVar "lFieldAtCorners"] ~accesses:[cField ()] ()] ()];
  let scaleSpeed d = Accesses.scale_immut ~factor:deltaT [nbMulti; ctx; cVarDef ("speed2" ^ d)] in
  !! List.iter scaleSpeed ["X"; "Y"; "Z"];
  !! Accesses.scale ~factor:deltaT [nbMulti; ctx; cReadOrWrite ~addr:[cAccesses ~base:[cVar "lParticles"] ~accesses:[cField ~field:"speed" (); cField ()] ()] ()];

  bigstep "finish with style";
  !! Variable.inline [ctx; cVarDefs ["accelX"; "accelY"; "accelZ"; "pos2X"; "pos2Y"; "pos2Z"]];
  !!! Arith.(simpls_rec [expand; gather_rec]) [ctx];
  !! Function.use_infix_ops ~indepth:true [ctx];
  !! Cleanup.std ();

  (* TODO:
    - cleanup script
    - local name tile: get elem_ty from program / resources
    - inlines: regroup together, problem: phase ordering matters, need fixpoint?
    - set_explicit: regroup
    - avoid binding fieldAtPosTmp
    - allow writing C code for constructing factors, need to parse and put in correct context with local ids
    - insert var for scaleFieldFactor
    - bind pointer to lParticles cell?

    !! Variable.bind_multi ~const:true ~is_ptr:true ~dest:[tBefore; cVarDef "p"] "paddr" [nbMulti; ctx; cCellAccess ~base:[cVar "lParticles"] ()];

    - inline cornerInterpolationCoeff to allow array on stack
    - FIXME: reparse triggers access normalization
  *)
)
