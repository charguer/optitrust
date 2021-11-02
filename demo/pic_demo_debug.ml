open Optitrust
open Target
open Ast 

let _ = Run.script_cpp (fun () ->
  
  (* Part: duplication of corners for vectorization of change deposit *)
  !! Matrix.intro_mops (Ast.trm_var "nbCells") [cVarDef "nextCharge"];
  !! Matrix.local_name ~my_mark:"first_local" ~var:"nextCharge" ~local_var:"nextChargeCorners" ~indices:["idCell"] [tIndex 1;cFunDef "main"; cFor "k"];
  !! Matrix_basic.delocalize ~dim:(trm_var "nbCorners") ~index:"k" ~acc:"sum" [cMark "first_local"];
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
  !! Sequence.insert (code my_bij_code) [tBefore;cFunDef "main"];
  (* !! Matrix.biject "mybij" [tIndex 0;cFunDef "main"; cFor "k" ; cFun "MINDEX2"]; *)
  
  (* !! Instr.replace (code "MINDEX2(nbCells, nbCorners, idCell2, k") [tIndex 1;cFunDef "main"; cFun "mybij"]; *)
  (* TODO: Replace mybij with MINDEX2 *)
  !! Instr.delete [tIndex 0; cFor "idCell" ~body:[sInstr "nextCharge["]];


  (* Part: duplication of corners for thread-independence of charge deposit #14 *)
  !! Variable.insert "nbProcs" "int" "8" [tBefore; cFunDef "main"];
  !! Matrix.local_name ~my_mark:"second_local" ~var:"nextChargeCorners" ~local_var:"nextChargeProCorners" ~indices:["idProc";"idCell"] [tIndex 2;cFunDef "main"; cFor "k"];
  !! Matrix_basic.delocalize ~dim:(trm_var "nbProcs") ~index:"k" ~acc:"sum" [cMark "second_local"];     
  !! Instr.delete [tIndex 0; cFor "idCell" ~body:[sInstr "nextChargeCorners["]];
  !! Specialize.any "k" [cAny];

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
  !! Loop.hoist [cVarDef "idCell2"];
  (* TODO: Loop.fission *)


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
  !! Variable.insert "blockSize" "int" "2" [tAfter; cVarDef "gridSize"];
  !! Variable.insert "d" "int" "1" [tAfter;cVarDef "gridSize"];
  !! Variable.insert "distanceToBlockLessThanHalfABlock" "bool"  
       "(ix >= bix + d && ix < bix + blockSize + d)
    && (iy >= biy + d && iy < biy + blockSize + d)
    && (iz >= biz + d && iz < biz + blockSize + d)" [tBefore; cFunDef "main"; cVarDef "coeffs"];
  !! Instr.replace (trm_var "distanceToBlockLessThanHalfABlock") [cFun "ANY_BOOL"];
  
  (* Part: Parallelization *)
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti;tBefore;cFor "idCell" ~body:[sInstr "sum +="]];
  !! Omp.parallel_for [Shared ["bx";"by";"bz"]] [tBefore; cFor "bx"];
  
  
  
)
