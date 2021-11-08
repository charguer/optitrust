open Optitrust
open Target

let _ = Run.script_cpp ~inline:["particle_chunk.h";"particle_chunk_alloc.h";"particle.h"] (fun () ->


  (* Part: inlining of the bag iteration *) (* skip #1 *)
  
  (* LATER: !! Function.bind_intro ~fresh_name:"r${occ}" ~const:true [nbMulti; cFun "vect_mul"]; *)

 (* Part1: space reuse *)
 (* TODO: Optimize *)
  !! Variable.reuse "p.speed" [cVarDef "speed2"];
  !! Variable.reuse "p.pos" [cVarDef "pos2"];
  

  (* Part: Introducing an if-statement for slow particles *)
  (* LATER: maybe name &bagsNext[idCell2]) *)
  !! Flow.insert_if "ANY_BOOL()" [cFunDef "main"; cFun "bag_push"];
  (* TODO: Optimize *)
  !! Instr.replace_fun "bag_push_serial" [cFunDef "main"; cIf ();dThen; cFun "bag_push"];
  !! Instr.replace_fun "bag_push_concurrent" [cFunDef "main"; cIf ();dElse; cFun "bag_push"];
  !! Function.inline [cFunDef "main"; cOr [[cFun "bag_push_serial"];[cFun "bag_push_concurrent"]]];

  (* Part: optimization of vect_matrix_mul *)
  !! Function.bind_intro ~fresh_name:"r1" ~const:true [cFunDef "vect_matrix_mul"; cFun "vect_mul"];
  !! Function.inline [cFunDef "vect_matrix_mul"; cOr [[cFun "vect_mul"];[cFun "vect_add"]]];
  !! Variable.inline [cFunDef "vect_matrix_mul"; cFor "k";cVarDef "r1"];
  !! Struct.set_explicit [nbMulti;cFunDef "vect_matrix_mul"; cWriteVar "result"];
  !! Loop.fission [nbMulti;tAfter; cFunDef "vect_matrix_mul"; cFor "k"; cFieldWrite ~base:[cVar "result"] ~regexp:true ~field:"[^z]" ()];
  !! Loop.unroll [nbMulti;cFunDef "vect_matrix_mul"; cFor "k"];
  (* TODO: Optimize *)
  !! Instr.accumulate ~nb:8 [tIndex 0; sInstr "result.x ="];
  !! Instr.accumulate ~nb:8 [tIndex 0; sInstr "result.y ="];
  !! Instr.accumulate ~nb:8 [tIndex 0; sInstr "result.z ="];
  !! Function.inline [cFun "vect_matrix_mul"];
  !! Variable.inline [cVarDef "fieldAtPos"];
  !! Variable.rename_on_block (ByList [("result1","fieldAtPos")]) [cFunDef "main"; cFor "i"; dBody];


  (* Part: vectorization of cornerInterpolationCoeff #2 *)
  !! Rewrite.equiv_at "double a; ==> a == (0. + 1. * a);" [nbMulti;cFunDef "cornerInterpolationCoeff"; cFieldWrite ~base:[cVar "r"] ~field:""(); dRHS; cVar ~regexp:true "r."];
  !! Variable.inline [nbMulti; cFunDef "cornerInterpolationCoeff";cVarDef ~regexp:true "c."];
  !! Variable.intro_pattern_array "double coef_x; double sign_x; double coef_y; double sign_y; double coef_z; double sign_z; ==>  double rx; double ry; double rz; ==> (coef_x + sign_x * rx) * (coef_y + sign_y * ry) * (coef_z + sign_z * rz);" [nbMulti;cFunDef "cornerInterpolationCoeff"; cFieldWrite ~base:[cVar "r"] ~field:""(); dRHS];
  !! Loop.fold ~index:"k" ~start:"0" ~stop:"nbCorners" ~step:"1" 8 [tIndex 0; cFieldWrite ~base:[cVar "r"] ~field:""()];

  (* Part: reveal fields *)
  !! Function.bind_intro ~fresh_name:"r2" ~const:true [tIndex ~nb:3 1; cFunDef "main"; cFun "vect_mul"];
  !! Function.bind_intro ~fresh_name:"r3" ~const:true [tIndex ~nb:3 2; cFunDef "main"; cFun "vect_mul"];
  !! Function.inline [cFunDef "main"; cOr [[cFun "vect_mul"];[cFun "vect_add"]]];
  !! Variable.inline [nbMulti; cFunDef "main"; cVarDef ~regexp:true "r."];
  !! Function.(inline ~vars:(AddSuffix "2"))[cFun "idCellOfPos"];
  !! Struct.set_explicit [cOr [[sInstr "p.speed ="];[sInstr "p.pos ="]]];
  !! Struct.set_explicit [nbMulti;sInstr "(c1->items)[index1] = "];
  !! Struct.set_explicit [nbMulti;cFunDef "main";cWrite ~typ:"vect" ()];
  !! Variable.inline [cOr [[cVarDef "p2"];[cVarDef "p"]]];
  !!! Struct.to_variables [cVarDef "fieldAtPos"];
  
  (* Part: optimization of accumulateChargeAtCorners *)
  !! Function.inline [ cOr [[cFun "vect8_mul"];[cFunDef "cornerInterpolationCoeff"; cFun ~regexp:true "relativePos."];
     [cVarDef "coeffs"; cFun "cornerInterpolationCoeff"];[cFun "accumulateChargeAtCorners"]]];
  !! Function.inline ~vars:(AddSuffix "2") [cFun "cornerInterpolationCoeff"];
  !! Variable.inline [cVarDef "deltaChargeOnCorners"];
     
     let mark = "mark_decls" in
  !! Marks.add mark [nbMulti;cFunDef "main"; 
      cOr [[cVarDef ~regexp:true ~substr:true "coef_.2"];
           [cVarDef ~regexp:true ~substr:true "sign_.2"]]];
     
  !! Variable.rename_on_block (ByList [
      ("coef_x2","coef_x");("coef_x1","coef_x");("coef_y2","coef_y");
      ("coef_y1","coef_y");("coef_z2","coef_z");("coef_z1","coef_z");
      ("sign_x2","sign_x");("sign_x1","sign_x");("sign_y2","sign_y");
      ("sign_y1","sign_y");("sign_z2","sign_z");("sign_z1","sign_z");]) [cFunDef "main"; cFor "i"; dBody];
     
     Instr.delete [nbMulti; cMark mark];
     Instr.move ~dest:[tBefore; cVarDef "rx1"] [nbMulti; cVarDef ~regexp:true "i.11"];
     Instr.move ~dest:[tBefore; cVarDef "rx2"] [nbMulti; cVarDef ~regexp:true "i.12"];
     Instr.move ~dest:[tBefore; cVarDef "r2"] [cOr [ [cVarDef ~regexp:true "indices1"];[cVarDef ~regexp:true "result1"]]];
  
  !! Instr.delete [cOr [[cVarDef "coeffs"];[cVarDef "coeffs2"]]];
  !! Variable.rename_on_block (ByList [("r1","coeffs");("r2","coeffs2")]) [cFunDef "main"; cFor "i"; dBody];
  !! Loop.fusion ~nb:3 [cFunDef "main"; cFor "k" ~body:[sInstr "coeffs2.values[k] ="]];
 !!! Instr.inline_last_write ~write:[sInstr "coeffs2.values[k] ="] [cRead ~addr:[sExpr "coeffs2.values"] ()];
  !! Instr.inline_last_write ~write:[sInstr "result1.values[k] ="] [cRead ~addr:[sExpr "result1.values"] ()];
  
  
  (* Part: scaling of speeds and positions #7 *)
  !! Variable.insert ~name:"factor"  ~typ:"const double" ~value:"particleCharge * stepDuration * stepDuration /particleMass / cellX" [tBefore; cVarDef "nbSteps"];
  !! Variable.insert ~name:"factorX" ~typ:"const double" ~value:"factor / cellX" [tAfter; cVarDef "factor"];
  !! Variable.insert ~name:"factorY" ~typ:"const double" ~value:"factor / cellY" [tAfter; cVarDef "factorX"];
  !! Variable.insert ~name:"factorZ" ~typ:"const double" ~value:"factor / cellZ" [tAfter; cVarDef "factorY"];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "factorX") [cVarDef "accel"; cReadVar "fieldAtPos_x"];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "factorY") [cVarDef "accel"; cReadVar "fieldAtPos_y"];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "factorZ") [cVarDef "accel"; cReadVar "fieldAtPos_z"];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "stepDuration / cellX") [nbMulti;cFunDef "main"; cWrite ~lhs:[sExpr "(c->items)[i].speed"] ();cRead ~addr:[sExpr "(c->items)[i].speed.x"] ()];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "stepDuration / cellY") [nbMulti;cFunDef "main"; cWrite ~lhs:[sExpr "(c->items)[i].speed"] ();cRead ~addr:[sExpr "(c->items)[i].speed.y"] ()];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "stepDuration / cellZ") [nbMulti;cFunDef "main"; cWrite ~lhs:[sExpr "(c->items)[i].speed"] ();cRead ~addr:[sExpr "(c->items)[i].speed.z"] ()];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "1. / cellX") [nbMulti;cFunDef "main"; cWrite ~lhs:[sExpr "(c->items)[i].pos"] ();cRead ~addr:[sExpr "(c->items)[i].pos.x"] ()];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "1. / cellY") [nbMulti;cFunDef "main"; cWrite ~lhs:[sExpr "(c->items)[i].pos"] ();cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "1. / cellZ") [nbMulti;cFunDef "main"; cWrite ~lhs:[sExpr "(c->items)[i].pos"] ();cRead ~addr:[sExpr "(c->items)[i].pos.z"] ()];
  
  (* TODO: arthur will give the pseudo code for the automated simplifier *)

  
  (* Part: grid_enumeration *)
  !! Loop.grid_enumerate [("ix", "gridSize"); ("iy", "gridSize"); ("iz", "gridSize")] [tIndex ~nb:3 1;cFor "idCell"];
  
  
  (* Part: Shifting of positions*)
  !! Instr.inline_last_write ~write:[cWriteVar "fieldAtPos_x"] [cVarDef "accel"; cRead ~addr:[cVar "fieldAtPos_x"] ()];
  !! Instr.inline_last_write ~write:[cWriteVar "fieldAtPos_y"] [cVarDef "accel"; cRead ~addr:[cVar "fieldAtPos_y"] ()];
  !! Instr.inline_last_write ~write:[cWriteVar "fieldAtPos_z"] [cVarDef "accel"; cRead ~addr:[cVar "fieldAtPos_z"] ()];
     
  !! Variable.inline [nbMulti;cVarDef ~regexp:true "fieldAtPos_."];
  !! Variable.inline [nbMulti; cFunDef "main"; cVarDef "accel"];
  !! Variable.bind_intro ~fresh_name:"px" [sInstr "(c->items)[i].pos.x ="; dRHS];
  !! Variable.bind_intro ~fresh_name:"py" [sInstr "(c->items)[i].pos.y ="; dRHS];
  !! Variable.bind_intro ~fresh_name:"pz" [sInstr "(c->items)[i].pos.z ="; dRHS];
  !! Instr.move_multiple ~destinations:[[tAfter; cVarDef "px"];[tAfter; cVarDef "py"]] ~targets:[[cVarDef "py"];[cVarDef "pz"]];
     
  !! Accesses.shift ~factor_ast:(Ast.trm_var "ix") [cOr [[cWrite ~lhs:[sExpr "(c->items)[i].pos.x"] ()]; [cVarDef "px"; cRead ~addr:[sExpr "(c->items)[i].pos.x"] ()]]];
  !! Accesses.shift ~factor_ast:(Ast.trm_var "iy") [cOr [[cWrite ~lhs:[sExpr "(c->items)[i].pos.y"] ()]; [cVarDef "py"; cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()]]];
  !! Accesses.shift ~factor_ast:(Ast.trm_var "iz") [cOr [[cWrite ~lhs:[sExpr "(c->items)[i].pos.z"] ()]; [cVarDef "pz"; cRead ~addr:[sExpr "(c->items)[i].pos.z"] ()]]];

  (* Part: convert pos fields to float *)
  !! Cast.insert (Ast.typ_float ()) [sInstr "(c->items)[i].pos.x ="; dRHS];
  !! Cast.insert (Ast.typ_float ()) [sInstr "(c->items)[i].pos.y ="; dRHS];
  !! Cast.insert (Ast.typ_float ()) [sInstr "(c->items)[i].pos.z ="; dRHS];

  (* Part: AOS-SOA *)
  !! Struct.inline "speed" [cTypDef "particle"];
  !! Struct.inline "pos" [cTypDef "particle"];

  (* Part: duplication of corners for vectorization of change deposit *)
  !! Matrix.intro_mops (Ast.trm_var "nbCells") [cVarDef "nextCharge"];
  !! Matrix.local_name ~my_mark:"first_local" ~var:"nextCharge" ~local_var:"nextChargeCorners" ~indices:["idCell"] [tIndex 1;cFunDef "main"; cFor "k"];
  !! Matrix_basic.delocalize ~dim:(Ast.trm_var "nbCorners") ~index:"k" ~acc:"sum" [cMark "first_local"];
  !! Variable.inline [cVarDef "indices1"];
  !! Specialize.any "k" [cAny];
  let my_bij_code = 
    "int mybij(int nbCells, int nbCorners, int idCell, int idCorner) {
      coord coord = coordOfCell(idCell);
      int ix = coord.ix;
      int iy = coord.iy; 
      int iz = coord.iz;
      int result[] = {
        cellOfCoord(ix, iy, iz),
        cellOfCoord(ix, iy, wrapX(gridZ,iz-1)),
        cellOfCoord(ix, wrapX(gridY,iy-1), iz),
        cellOfCoord(ix, wrapX(gridY,iy-1), wrapX(gridZ,iz-1)),
        cellOfCoord(wrapX(gridX,ix-1), iy, iz),
        cellOfCoord(wrapX(gridX,ix-1), iy, wrapX(gridZ,iz-1)),
        cellOfCoord(wrapX(gridX,ix-1), wrapX(gridY,iy-1), iz),
        cellOfCoord(wrapX(gridX,ix-1), wrapX(gridY,iy-1), wrapX(gridZ,iz-1)),
      };
     return MINDEX2(nbCells, nbCorners, result[idCorner], idCorner);
     }" in
    Sequence.insert (Ast.code my_bij_code) [tBefore;cFunDef "main"];
!!! Matrix.biject "mybij" [tIndex 0;cFunDef "main"; cFor "k" ; cFun "MINDEX2"];
  !! Instr.delete [tIndex 0; cFor "idCell" ~body:[sInstr "nextCharge["]];
  !! Instr.replace (Ast.code "MINDEX2(nbCells, nbCorners, idCell2,k)") [cFun "mybij"];

  (* Part: duplication of corners for thread-independence of charge deposit #14 *)
  !! Variable.insert ~name:"nbProcs" ~typ:"int" ~value:"8" [tBefore; cFunDef "main"];
  !! Matrix.local_name ~my_mark:"second_local" ~var:"nextChargeCorners" ~local_var:"nextChargeProCorners" ~indices:["idProc";"idCell"] [tIndex 2;cFunDef "main"; cFor "k"];
  !! Matrix_basic.delocalize ~dim:(Ast.trm_var "nbProcs") ~index:"k" ~acc:"sum" [cMark "second_local"];     
  !! Instr.delete [tIndex 0; cFor "idCell" ~body:[sInstr "nextChargeCorners["]];
  !! Specialize.any "k" [cAny];

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
  !! Sequence.intro ~mark:"temp_seq" ~start:[cFunDef "main";cVarDef "coef_x"] ~nb:6 (); 
  !! Instr.move_invariant ~dest:[tBefore; cFunDef "main"] [cMark "temp_seq"];
  !! Sequence.elim [cMark "temp_seq"];
  !! Loop.fission [tBefore; cVarDef "px"];
  !! Loop.fission [tBefore; cVarDef "ix2"];
  !! Loop.hoist [cVarDef "idCell2"];


  (* Part: Coloring *)
     let sized_dims = [("ix", "gridX"); ("iy", "gridY"); ("iz", "gridZ")] in
     let dims = List.map fst sized_dims in
     let colorize (tile : string) (color : string) (d:string) : unit =
      let bd = "b" ^ d in
      Loop_basic.tile tile ~bound:TileBoundDivides ~index:"b${id}" [cFor d]; (* DONE: ~index:"b${id}" *)
      Loop_basic.color color ~index:("c"^d) [cFor bd]
        in
  !! List.iter (colorize "2" "2") dims;
  !! Loop.reorder ~order:(Tools.((add_prefix "c" dims) @ (add_prefix "b" dims) @ dims)) [cFor "cix"];
  
  (* Introduction of the computation *)
  !! Variable.insert ~name:"blockSize" ~typ:"int" ~value:"2" [tAfter; cVarDef "gridSize"];
  !! Variable.insert ~name:"d" ~typ:"int" ~value:"blockSize / 2" [tAfter;cVarDef "blockSize"];
  !! Variable.insert ~name:"distanceToBlockLessThanHalfABlock" ~typ:"bool"  ~value:"(ix >= bix + d && ix < bix + blockSize + d)&& (iy >= biy + d && iy < biy + blockSize + d) && (iz >= biz + d && iz < biz + blockSize + d)" [tAfter; cVarDef "rz1"];
  !! Instr.replace (Ast.trm_var "distanceToBlockLessThanHalfABlock") [cFun "ANY_BOOL"];
  
  
  (* Part: Parallelization *)
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti;tBefore;cFor "idCell" ~body:[sInstr "sum +="]];
  !! Omp.parallel_for [Shared ["bx";"by";"bz"]] [tBefore; cFor "bix"]; 

  (* Part: optimize chunk allocation *)
  (* skip #16 *)


)

