open Optitrust
open Target
open Ast 

let _ = Run.script_cpp (fun () ->

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
