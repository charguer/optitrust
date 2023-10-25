
open Optitrust
open Target
(*
let _ = Run.script_cpp (fun _->

  (* Example with pointers *)
  !! Variable_basic.fold ~at:[cVarDef "r1"] [cVarDef "y"];
  (* Example with arrays *)
  !! Variable_basic.fold [cVarDef "a"];
  (* Example with matrices *)
  !! Variable_basic.fold ~at:[cVarDef "r3"] [cVarDef "b"];
  (* show [sInstr "m[1][1] = 9"]; *)
  !! Variable_basic.fold ~at:[cCellWrite ()] [cVarDef "b"];

)
*)
