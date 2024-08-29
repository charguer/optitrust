open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

(** Reproducing a subset of the PIC case study *)

let _ = Run.script_cpp (fun () ->
  let ctx = cFunBody "simulate_single_cell" in
  !! Resources.ensure_computed ();

  bigstep "make local copies of field and particles";
  (* let fv n = find_var_in_current_ast ~target:[ctx] n in *)
  let ft n = typ_var (find_var_in_current_ast_filter ~target:[ctx] (fun v -> v.name = n)) in
  (* TODO: get type and dims from resources *)
  !! Matrix.local_name_tile ~var:"fieldAtCorners"
    (* ~type_and_dims:(ft "vect", [trm_var (fv "nbCorners")]) *)
    ~elem_ty:(ft "vect") ~uninit_post:true
    ~local_var:"lFieldAtCorners" [ctx; cFor "idStep"];
  !! Matrix.local_name_tile ~var:"particles"
    (* ~type_and_dims:(ft "particle", [trm_var (fv "nbParticles")]) *)
    ~elem_ty:(ft "particle")
    ~local_var:"lParticles" [ctx; cFor "idStep"];

  bigstep "inline helper functions and reveal record fields";
  !! Function.inline [ctx; multi cFun ["matrix_vect_mul"]];
  (* TODO: regroup with previous inline, problem: phase ordering matters, need fixpoint? *)
  !! Function.inline [ctx; multi cFun ["vect_add"; "vect_mul"]];

  (* TODO: regroup, set_explicit_all *)
  !! Record.set_explicit [ctx; multi cArrayWrite ["particles"; "lParticles"]];
  !! Record.set_explicit [nbMulti; ctx; cWrite ~lhs:[Constr_depth (DepthAt 0); cAccesses ~base:[cOr (List.map (fun x -> [cVar x]) ["particles"; "lParticles"])] ~inner_accesses:false ~accesses:[cIndex (); cField ~regexp:true ~field:"\\(pos\\)\\|\\(speed\\)" ()] ()] ()];
  !! Record.set_explicit [ctx; multi cArrayWrite ["fieldAtCorners"; "lFieldAtCorners"]];
  (* TODO: avoid binding tmp *)
  !! Variable.bind ~const:true "fieldAtPosTmp" [cWriteVar "fieldAtPos"; dArg 1];
  !! Record.set_explicit [ctx; cWriteVar "fieldAtPos"];
  !! Record.to_variables [ctx; cVarDefs ["fieldAtPosTmp"; "fieldAtPos"; "pos2"; "speed2"; "accel"]];
  (* TODO:
  !! List.iter (fun d -> Variable.inline [cVarDef ("fieldAtPosTmp" ^ d)]) ["X"; "Y"; "Z"]; *)

  (* CHECK *)
  bigstep "scale field and particles";
  (*
  let p = trm_var (find_var_in_current_ast ~target:[ctx] "p") in
   (trm_struct_get p "charge")
   (trm_struct_get p "mass")
  *)
  (* TODO: allow writing C code, need to parse and put in correct context with local ids *)
  let pCharge = trm_var (find_var_in_current_ast ~target:[ctx] "pCharge") in
  let pMass = trm_var (find_var_in_current_ast ~target:[ctx] "pMass") in
  let deltaT = trm_var (find_var_in_current_ast ~target:[ctx] "deltaT") in
  let scaleFieldFactor = trm_mul (trm_div pCharge pMass) (trm_mul deltaT deltaT) in
  (* TODO: Insert var for scaleFieldFactor *)
  let scaleField d = Accesses.scale ~factor:scaleFieldFactor [nbMulti; ctx; cVar ("fieldAtPos" ^ d)] in
  !! List.iter scaleField ["X"; "Y"; "Z"];
  !! Accesses.scale ~factor:scaleFieldFactor [nbMulti; ctx; cReadOrWrite ~addr:[cAccesses ~base:[cVar "lFieldAtCorners"] ~accesses:[cField ()] ()] ()];
  let scaleSpeed d = Accesses.scale_immut ~factor:deltaT [nbMulti; ctx; cVarDef ("speed2" ^ d)] in
  !! List.iter scaleSpeed ["X"; "Y"; "Z"];
  (* !! Variable.inline [cVarDef "p"]; TODO: think about this *)
  (* !! Variable.bind_multi ~const:true ~is_ptr:true ~dest:[tBefore; cVarDef "p"] "paddr" [nbMulti; ctx; cCellAccess ~base:[cVar "lParticles"] ()]; *)

  !!! (); (* FIXME: why are things not found without reparse ? *)
  let deltaT = trm_var (find_var_in_current_ast ~target:[ctx] "deltaT") in

  !! Accesses.scale ~factor:deltaT [nbMulti; ctx; cReadOrWrite ~addr:[cAccesses ~base:[cVar "lParticles"] ~accesses:[cField ~field:"speed" (); cField ()] ()] ()];

  bigstep "finish with style";
  !! Variable.inline [ctx; cVarDefs ["accelX"; "accelY"; "accelZ"; "pos2X"; "pos2Y"; "pos2Z"]];
  !!! Arith.(simpls_rec [expand; gather_rec]) [ctx];
  !! Function.use_infix_ops ~indepth:true [ctx];
  !! Cleanup.std ();

  (* TODO:
    - cleanup script
    - bind pointer to lParticles cell?
    - inline cornerInterpolationCoeff to allow array on stack
  *)
)
