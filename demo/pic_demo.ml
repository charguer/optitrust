open Optitrust
open Target

let _ = Run.script_cpp ~inline:["particle_chunk.h";"particle_chunk_alloc.h";"particle.h"] (fun () ->

  (* Part: inlining of the bag iteration *)
  (* skip #1 *)




  (* Part: optimize chunk allocation *)
  (* skip #16 *)

  (* PART: Inlining of arithmetic operations *)
  (* TODO: the intermediate names should be inserted then inlined automatically *)

  (* LATER: !! Function.bind_intro ~fresh_name:"r${occ}" ~const:true [nbMulti; cFun "vect_mul"]; *)

  
  
  

  (* Part: Introducing an if-statement for slow particles *)
  !! Flow.insert_if "ANY_BOOL()" [cFunDef "main"; cFun "bag_push"];
  !! Instr.replace_fun "bag_push_serial" [cFunDef "main"; cIf ();dThen; cFun "bag_push"];
  !! Instr.replace_fun "bag_push_concurrent" [cFunDef "main"; cIf ();dElse; cFun "bag_push"];
  !! Function.inline [cFunDef "main";cFun "bag_push_serial"];
  !! Function.inline [cFunDef "main";cFun "bag_push_concurrent"];

  (* Part: space reuse *)
  !! Variable.reuse "p.speed" [cVarDef "speed2"];
  !! Variable.reuse "p.pos" [cVarDef "pos2"];
  

  (* Part: optimization of vect_matrix_mul *)
  !! Function.bind_intro ~fresh_name:"r1" ~const:true [cFunDef "vect_matrix_mul"; cFun "vect_mul"];
  !! Function.inline [cFunDef "vect_matrix_mul"; cFun "vect_mul"];
  !! Function.inline [cFunDef "vect_matrix_mul"; cFun "vect_add"];
  !! Variable.inline [cFunDef "vect_matrix_mul"; cFor "k";cVarDef "r1"];
  !! Struct.set_explicit [nbMulti;cFunDef "vect_matrix_mul"; cWriteVar "result"];
  (* TODO: Remove braces when the block parameter is empty *)
  !! Loop.unroll [nbMulti;cFunDef "vect_matrix_mul"; cFor "k"];
  !! Function.inline [cFun "vect_matrix_mul"];
  !! Variable.inline [cVarDef "fieldAtPos"];
  !! Variable.rename_on_block (ByList [("result1","fieldAtPos")]) [cFunDef "main"; cFor "i"; dBody];

  
  (* Part: vectorization of cornerInterpolationCoeff #2 *)
  !! Rewrite.equiv_at "double a; ==> a == (0. + 1. * a);" [nbMulti;cFunDef "cornerInterpolationCoeff"; cReturn; cVar ~regexp:true "r."];
  !! Variable.inline [nbMulti; cFunDef "cornerInterpolationCoeff";cVarDef ~regexp:true "c."];
  !!! Variable.intro_pattern_array "double coef_x; double sign_x; double coef_y; double sign_y; double coef_z; double sign_z; ==>  double rx; double ry; double rz; ==> (coef_x + sign_x * rx) * (coef_y + sign_y * ry) * (coef_z + sign_z * rz);" [nbMulti;cReturn; cCell ()];
  !! Variable.bind_intro ~fresh_name:"values" [cFunDef "cornerInterpolationCoeff"; cReturn; cArrayInit];
  !! Arrays.set_explicit [cFunDef "cornerInterpolationCoeff";cVarDef "values"];
  !! Loop.fold ~index:"k" ~start:"0" ~stop:"nbCorners" ~step:"1" 8 [cCellWrite ~base:[cVar "values"] ~index:[cInt 0]];
  
  
  (* Part: reveal fields *)
  !! Function.bind_intro ~fresh_name:"r2" ~const:true [tIndex ~nb:3 1; cFunDef "main"; cFun "vect_mul"];
  !! Function.bind_intro ~fresh_name:"r3" ~const:true [tIndex ~nb:3 2; cFunDef "main"; cFun "vect_mul"];
  !! Function.inline [nbMulti;cFunDef "main"; cFun "vect_mul"];
  !! Function.inline [nbMulti;cFunDef "main"; cFun "vect_add"];
  !! Variable.inline [nbMulti; cFunDef "main"; cVarDef "r2"];  
  !! Variable.inline [nbMulti; cFunDef "main"; cVarDef "r3"];  
  !! Function.(inline ~vars:(AddSuffix "2"))[cFun "idCellOfPos"];
  !! Struct.set_explicit [sInstr "p.speed ="];
  !! Struct.set_explicit [sInstr "p.pos ="];
  !! Struct.set_explicit [nbMulti;sInstr "(c1->items)[index1] = "];
  !! Struct.set_explicit [nbMulti;cFunDef "main";cWrite ~typ:"vect" ()];
  !! Variable.inline [cVarDef "p2"];
  !! Variable.inline [cVarDef "p"];
  !!! Variable.inline [nbMulti; cFunDef "main"; cVarDef "accel"]; 
   (*TODO: Update inline last write so that it works without giving a specific target on the read  *)


  (* Part: optimization of accumulateChargeAtCorners *)
  !! Function.inline [cFun "vect8_mul"];
  !! Variable.inline [cVarDef "deltaChargeOnCorners"];
  !! Function.inline [cVarDef "coeffs2"; cFun "cornerInterpolationCoeff"]; 
  !! Instr.replace (Ast.trm_var "values1") [cFieldAccess ~base:[cVar "coeffs2"] ~field:"values" ()];
  !! Function.inline [cFun "accumulateChargeAtCorners"];
  !! Function.inline [nbMulti;cFunDef "main"; cFun ~regexp:true "relativePos."];
  !! Instr.move ~target:[tBefore; cVarDef "rx1"] [cVarDef "iy1"];
  !! Instr.move ~target:[tBefore; cVarDef "rx1"] [cVarDef "iz1"];
  
  (* !! Loop.fusion ~nb:2 [tIndex ~nb:2 0; cFunDef "main"; cFor "k"];
  !!! Instr.inline_last_write ~write:[sInstr "result1.values[k] ="] [cRead ~addr:[sExpr "result1.values"] ()];
  !! Loop.unroll ~braces:false [cFunDef "main";cFor "k"]; *)
  

  (* Part: scaling of speeds and positions #7 *)
  !! Variable.insert "factor"  "const double" "particleCharge * stepDuration * stepDuration /particleMass / cellX" [tBefore; cVarDef "nbSteps"];
  !! Variable.insert "factorX" "const double" "factor / cellX" [tAfter; cVarDef "factor"];
  !! Variable.insert "factorY" "const double" "factor / cellY" [tAfter; cVarDef "factorX"];
  !! Variable.insert "factorZ" "const double" "factor / cellZ" [tAfter; cVarDef "factorY"];
  !! Accesses.scale (Ast.trm_var "factorX") [sInstr "c->items";cFieldRead ~field:"x" ~base:[cVar "fieldAtPos"] ()];
  !! Accesses.scale (Ast.trm_var "factorY") [sInstr "c->items";cFieldRead ~field:"y" ~base:[cVar "fieldAtPos"] ()];
  !! Accesses.scale (Ast.trm_var "factorZ") [sInstr "c->items";cFieldRead ~field:"x" ~base:[cVar "fieldAtPos"] ()];
  
  (* TODO: Find the right target for matching accesses of speed *)
  (* !! Accesses.scale (Ast.trm_var "stepDuration / cellX") [sInstr "c->items";cFieldRead ~field:"x" ~base:[sExpr "c->items.speed"] ()]; *)
  (* !! Accesses.scale (Ast.trm_var "stepDuration / cellY") [sInstr "c->items";cFieldRead ~field:"y" ~base:[sExpr "c->items.speed"] ()]; *)
  (* !! Accesses.scale (Ast.trm_var "stepDuration / cellZ") [sInstr "c->items";cFieldRead ~field:"x" ~base:[sExpr "c->items.speed"] ()]; *)
  
  (* Part: shifting of positions #8  *)
  !! Function.bind_args ["px"] [cFunDef "main"; tIndex ~nb:3 0; cFun "int_of_double"];
  !! Function.bind_args ["py"] [cFunDef "main"; tIndex ~nb:3 1; cFun "int_of_double"];
  !! Function.bind_args ["pz"] [cFunDef "main"; tIndex ~nb:3 2; cFun "int_of_double"];
  !! Instr.move ~target:[tAfter; cVarDef "pz"] [cVarDef "iy2"];
  !! Instr.move ~target:[tAfter; cVarDef "pz"] [cVarDef "ix2"];


  

  (* TODO: missing the type in the generatino of:
     const r0 = vect_mul(coeffs.values[k], matrix.values[k]);
 in:
  !! Function.bind_intro ~fresh_name:"r0" ~const:true [cFunDef "vect_matrix_mul"; cFun "vect_mul"];
*)
  
  (* TODO: at the combi level it should work *)

  (* Part: AOS-TO-SOA -- TODO: this does not work, it seems that
        result.values[k].x = fields[indices.values[k]].x;
        is incorrectly targeted when looking for field "pos" of type "particle" :
          !!! Struct.inline "pos" [cTypDef "particle"];
 *)
  (*
  !!! Struct.inline "speed" [cTypDef "particle"];
  !!! Struct.inline "items" [cTypDef "bag"];
  *)

  (* LATER: probably not needed
  !!! Function.bind_args ["auto ppos"] [cVarDef "coeffs"; cFun "cornerInterpolationCoeff"]; LATER: should insert the type *)



  


  (* Part: scaling of electric field -- LATER ARTHUR: check if we need this *)


  

  (* Part: optimization of accumulateChargeAtCorners #10 *)

  (* Part: introduction of matrix macros #12 *)

  (* Part: duplication of corners for vectorization of charge deposit #13 *)

  (* Part: duplication of corners for thread-independence of charge deposit #14 *)

  (* Part: loop splitting for treatments of speeds and positions and deposit #11 *)

    (* TODO: adapt the script below; the hoisting should be on the variable idCell2 *)
    !! Struct.to_variables [cVarDef "speed2"];
    !! Loop.hoist ~name:"${var}_at" [nbMulti; cVarDef ~regexp:true "speed2_."];
    (* remove the dollar to see the error *)
    !! Variable.inline [nbMulti; cVarDef ~regexp:true "speed2_.$"];
    !! Loop.fission [tBefore; cVarDef "pos2"];

  (* Part: Coloring *)
  (* TODO: find a better target *)
  let sized_dims = [("x", "gridX"); ("y", "gridY"); ("z", "gridZ")] in
  let dims = List.map fst sized_dims in
  !! Loop.grid_enumerate sized_dims [tIndex ~nb:2 0; cFor "idCell"];
  let colorize (tile : string) (color : string) (d:string) : unit =
    let bd = "b" ^ d in
    Loop_basic.tile tile ~bound:TileBoundDivides ~index:"b${id}" [cFor d]; (* DONE: ~index:"b${id}" *)
    Loop_basic.color color ~index:("c"^d) [cFor bd]
    in
  !! List.iter (colorize "2" "2") dims;
  !! Loop.reorder ~order:(Tools.((add_prefix "c" dims) @ (add_prefix "b" dims) @ dims)) [cFor "cx"];

  (* Part: Introducing an if-statement for slow particles *)
  (*
  !! Function.inline [cFunDef "bag_transfer"; cFun "bag_push"];
  !! Function.inline ~args:["b2";""] [cTopFunDef "main"; cFun "bag_push"];
  !! Variable.insert_and_fold "k" "int&" "b2.nb" [tAfter; cVarDef "b2"];
  *)

  (* Part: Parallelization *)
  !! Omp.parallel_for [Shared ["bx";"by";"bz"]] [tBefore; cFor "bx"];
  (* TODO: a few more parallel_for to add *)

  )

