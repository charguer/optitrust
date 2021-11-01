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

 (* Part1: space reuse *)
  !! Variable.reuse "p.speed" [cVarDef "speed2"];
     Variable.reuse "p.pos" [cVarDef "pos2"];
  

  (* Part: Introducing an if-statement for slow particles *)
  (* LATER: maybe name &bagsNext[idCell2]) *)
  !! Flow.insert_if "ANY_BOOL()" [cFunDef "main"; cFun "bag_push"];
     Instr.replace_fun "bag_push_serial" [cFunDef "main"; cIf ();dThen; cFun "bag_push"];
     Instr.replace_fun "bag_push_concurrent" [cFunDef "main"; cIf ();dElse; cFun "bag_push"];
     Function.inline [cFunDef "main";cFun "bag_push_serial"];
     Function.inline [cFunDef "main";cFun "bag_push_concurrent"];


  (* Part: optimization of vect_matrix_mul *)
  !! Function.bind_intro ~fresh_name:"r1" ~const:true [cFunDef "vect_matrix_mul"; cFun "vect_mul"];
     Function.inline [cFunDef "vect_matrix_mul"; cOr [[cFun "vect_mul"];[cFun "vect_add"]]];
     Variable.inline [cFunDef "vect_matrix_mul"; cFor "k";cVarDef "r1"];
     Struct.set_explicit [nbMulti;cFunDef "vect_matrix_mul"; cWriteVar "result"];
     Loop.fission [nbMulti;tAfter; cFunDef "vect_matrix_mul"; cFor "k"; cFieldWrite ~base:[cVar "result"] ~regexp:true ~field:"[^z]" ()];
     Loop.unroll [nbMulti;cFunDef "vect_matrix_mul"; cFor "k"];
     (* TODO:  update inline last write so that it finds the read target *)
     Function.inline [cFun "vect_matrix_mul"];
     Variable.inline [cVarDef "fieldAtPos"];
     Variable.rename_on_block (ByList [("result1","fieldAtPos")]) [cFunDef "main"; cFor "i"; dBody];


  (* Part: vectorization of cornerInterpolationCoeff #2 *)
  !! Rewrite.equiv_at "double a; ==> a == (0. + 1. * a);" [nbMulti;cFunDef "cornerInterpolationCoeff"; cFieldWrite ~base:[cVar "r"] ~field:""(); dRHS; cVar ~regexp:true "r."];
     Variable.inline [nbMulti; cFunDef "cornerInterpolationCoeff";cVarDef ~regexp:true "c."];
  !!! Variable.intro_pattern_array "double coef_x; double sign_x; double coef_y; double sign_y; double coef_z; double sign_z; ==>  double rx; double ry; double rz; ==> (coef_x + sign_x * rx) * (coef_y + sign_y * ry) * (coef_z + sign_z * rz);" [nbMulti;cFunDef "cornerInterpolationCoeff"; cFieldWrite ~base:[cVar "r"] ~field:""(); dRHS];
     Loop.fold ~index:"k" ~start:"0" ~stop:"nbCorners" ~step:"1" 8 [tIndex 0; cFieldWrite ~base:[cVar "r"] ~field:""()];

  (* Part: reveal fields *)
  !! Function.bind_intro ~fresh_name:"r2" ~const:true [tIndex ~nb:3 1; cFunDef "main"; cFun "vect_mul"];
     Function.bind_intro ~fresh_name:"r3" ~const:true [tIndex ~nb:3 2; cFunDef "main"; cFun "vect_mul"];
     Function.inline [cFunDef "main"; cOr [[cFun "vect_mul"];[cFun "vect_add"]]];
     Variable.inline [nbMulti; cFunDef "main"; cVarDef ~regexp:true "r."];
     Function.(inline ~vars:(AddSuffix "2"))[cFun "idCellOfPos"];
     Struct.set_explicit [cOr [[sInstr "p.speed ="];[sInstr "p.pos ="]]];
     Struct.set_explicit [nbMulti;sInstr "(c1->items)[index1] = "];
     Struct.set_explicit [nbMulti;cFunDef "main";cWrite ~typ:"vect" ()];
     Variable.inline [cOr [[cVarDef "p2"];[cVarDef "p"]]];
  !!! Variable.inline [nbMulti; cFunDef "main"; cVarDef "accel"];
  (* TODO: Fix Variable.inline for struct fields inisde get operations *)
   (*TODO: Update inline last write so that it works without giving a specific target on the read  *)


  (* Part: optimization of accumulateChargeAtCorners *)
  !! Function.inline [cFun "vect8_mul"];
     Variable.inline [cVarDef "deltaChargeOnCorners"];
     Function.inline [nbMulti;cFunDef "cornerInterpolationCoeff"; cFun ~regexp:true "relativePos."];
     Function.inline [cVarDef "coeffs"; cFun "cornerInterpolationCoeff"];
     Function.inline ~vars:(AddSuffix "2") [cFun "cornerInterpolationCoeff"];
     Function.inline [cFun "accumulateChargeAtCorners"];
     Instr.move ~target:[tBefore; cVarDef "rx1"] [cVarDef "iy11"];
     Instr.move ~target:[tBefore; cVarDef "rx1"] [cVarDef "iz11"];
     Instr.move ~target:[tBefore; cVarDef "rx2"] [cVarDef "iy12"];
     Instr.move ~target:[tBefore; cVarDef "rx2"] [cVarDef "iz12"];
     Instr.move ~target:[tAfter; cVarDef "r2"] [cVarDef "result1"];
     Instr.move ~target:[tAfter; cVarDef "r2"] [cVarDef "indices1"];
     Instr.delete [cOr [[cVarDef "coeffs"];[cVarDef "coeffs2"]]];
     Variable.rename_on_block (ByList [("r1","coeffs");("r2","coeffs2")]) [cFunDef "main"; cFor "i"; dBody];
     Loop.fusion ~nb:3 [cFunDef "main"; cFor "k" ~body:[sInstr "coeffs2.values[k] ="]];
 !!! Instr.inline_last_write ~write:[sInstr "coeffs2.values[k] ="] [cRead ~addr:[sExpr "coeffs2.values"] ()];
     Instr.inline_last_write ~write:[sInstr "result1.values[k] ="] [cRead ~addr:[sExpr "result1.values"] ()];
     Loop.unroll ~braces:false [tIndex 1;cFunDef "main";cFor "k"];
  
  (* Part: scaling of speeds and positions #7 *)
  !! Variable.insert "factor"  "const double" "particleCharge * stepDuration * stepDuration /particleMass / cellX" [tBefore; cVarDef "nbSteps"];
     Variable.insert "factorX" "const double" "factor / cellX" [tAfter; cVarDef "factor"];
     Variable.insert "factorY" "const double" "factor / cellY" [tAfter; cVarDef "factorX"];
     Variable.insert "factorZ" "const double" "factor / cellZ" [tAfter; cVarDef "factorY"];
     Accesses.scale (Ast.trm_var "factorX") [sInstr "c->items";cFieldRead ~field:"x" ~base:[cVar "fieldAtPos"] ()];
     Accesses.scale (Ast.trm_var "factorY") [sInstr "c->items";cFieldRead ~field:"y" ~base:[cVar "fieldAtPos"] ()];
     Accesses.scale (Ast.trm_var "factorZ") [sInstr "c->items";cFieldRead ~field:"x" ~base:[cVar "fieldAtPos"] ()];
     Accesses.scale (Ast.trm_var "stepDuration / cellX") [sInstr "(c->items)[i].pos.x ="; cRead ~addr:[sExpr "(c->items)[i].speed.x"] ()];
     Accesses.scale (Ast.trm_var "stepDuration / cellY") [sInstr "(c->items)[i].pos.y ="; cRead ~addr:[sExpr "(c->items)[i].speed.y"] ()];
     Accesses.scale (Ast.trm_var "stepDuration / cellZ") [sInstr "(c->items)[i].pos.z ="; cRead ~addr:[sExpr "(c->items)[i].speed.z"] ()];
  (* TODO: More scaling operations needed *)
  (* TODO: arthur will give the pseudo code for the automated simplifier *)

  (* NOTE:
  Accesses.scale ~factor:"factorX"
  Accesses.scale ~factor_ast:(Ast.trm_var "factorX")

    let f (factor:strm) (factor_ast:trm)
  let factor : trm = combine_strm ~name:"factor" factor factor_ast in

  combine_strm ~default:None => error (raise exception (sprintf "please don't provide %s and %s_ast together" $name sname) if none provided
  combine_strm ~default:(Some "foo")  => use default if none provided
  *)
  (* Part: grid_enumeration *)
  !! Loop.grid_enumerate [("ix", "gridSize"); ("iy", "gridSize"); ("iz", "gridSize")] [tIndex ~nb:3 1;cFor "idCell"];

  (* Part: shifting of positions #8  *)
 !! Function.bind_args ["px2"] [cFunDef "main"; tIndex ~nb:6 0; cFun "int_of_double"];
    Function.bind_args ["py2"] [cFunDef "main"; tIndex ~nb:6 1; cFun "int_of_double"];
    Function.bind_args ["pz2"] [cFunDef "main"; tIndex ~nb:6 2; cFun "int_of_double"];
    Instr.move ~target:[tAfter; cVarDef "pz2"] [cVarDef "iy2"];
    Instr.move ~target:[tAfter; cVarDef "pz2"] [cVarDef "ix2"];
    (* LATER: ARTHUR will figure out how to do this in one step, by allowing regexp capture in transfos. *)
    (* !! Accesses.shift (Ast.trm_var "coordOfCell(idCell).ix") [sInstr "(c->items)[i].pos.x ="];
    !! Accesses.shift (Ast.trm_var "coordOfCell(idCell).iy") [sInstr "(c->items)[i].pos.y ="];
    !! Accesses.shift (Ast.trm_var "coordOfCell(idCell).iz") [sInstr "(c->items)[i].pos.z ="];
    !! Accesses.shift (Ast.trm_var "coordOfCell(idCell).ix") [cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()];
    !! Accesses.shift (Ast.trm_var "coordOfCell(idCell).iy") [cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()];
    !! Accesses.shift (Ast.trm_var "coordOfCell(idCell).iz") [cRead ~addr:[sExpr "(c->items)[i].pos.z"] ()]; *)
    (* !!! (); Instead of reparsing for each transformation applied we do a single reparse at the end of shifting *)
    (* LATER
    !! Accesses.shift (Ast.trm_var "i${occ[1]}") [sInstr ~regexp:true "(c->items)\[i\].pos.\.\) ="];*)
    Accesses.shift (Ast.trm_var "ix") [sInstr "(c->items)[i].pos.x ="];
    Accesses.shift (Ast.trm_var "iy") [sInstr "(c->items)[i].pos.y ="];
    Accesses.shift (Ast.trm_var "iz") [sInstr "(c->items)[i].pos.z ="];
    Accesses.shift (Ast.trm_var "ix") [nbMulti;cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()];
    Accesses.shift (Ast.trm_var "iy") [nbMulti;cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()];
    Accesses.shift (Ast.trm_var "iz") [nbMulti;cRead ~addr:[sExpr "(c->items)[i].pos.z"] ()];

 (* Trace.dump ~prefix:"pic_demo_wip" *)
 (* TODO: create   pic_demo_wip_out.ml *)
 (* TODO: create   pic_demo_after.ml
     cp pic_demo_after.ml pic_demo_wip.ml  *)

  (* Part: convert pos fields to float *)
  !! Cast.insert (Ast.typ_float ()) [sInstr "(c->items)[i].pos.x ="; dRHS];
     Cast.insert (Ast.typ_float ()) [sInstr "(c->items)[i].pos.y ="; dRHS];
     Cast.insert (Ast.typ_float ()) [sInstr "(c->items)[i].pos.z ="; dRHS];

  (* Part: AOS-SOA *)
  !! Struct.inline "speed" [cTypDef "particle"];
     Struct.inline "pos" [cTypDef "particle"];
  (* !!! Struct.inline "items" [cTypDef "chunk"]; *) (* Fix me! *)


  (* Part: introduction of matrix macros *)
  !! Matrix.intro_mops (Ast.trm_var "nbCells") [cVarDef "nextCharge"];

  (* Part: duplication of corners for vectorization of charge deposit *)
  (* TOOD: WIP *)





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

