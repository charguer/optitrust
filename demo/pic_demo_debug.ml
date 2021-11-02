open Optitrust
open Target
open Ast 

let _ = Run.script_cpp (fun () ->
  
  (* Part: duplication of corners for vectorization of change deposit *)
  !! Matrix.intro_mops (Ast.trm_var "nbCells") [cVarDef "nextCharge"];
  !! Matrix.local_name ~my_mark:"first_local" ~var:"nextCharge" ~local_var:"nextChargeCorners" [tIndex 1;cFunDef "main"; cFor "k"];
  !! Matrix_basic.delocalize ~dim:(trm_var "nbCorners") ~index:"i0" ~acc:"sum" [cMark "first_local"];
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
  (* TODO: Replace mybij with MINDEX2 *)
  
  (* Part: duplication of corners for thread-independence of charge deposit #14 *)
  
  !! Matrix.local_name ~my_mark:"second_local" ~var:"nextChargeCorners" ~local_var:"nextChargeProCorners" [tIndex 1;cFunDef "main"; cFor "k"];
  !! Matrix_basic.delocalize ~dim:(trm_var "nbProc") ~index:"idProc" ~acc:"sum" [cMark "second_local"];     
  !! Omp.parallel_for [Shared ["idCell"]] [tBefore;tIndex 1; cFor "k"];
  

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
  
)
