open Optitrust
open Target


let _ = Run.script_cpp (fun _->

  (* Fold everywhere *)
  !! Variable_basic.fold [cVarDef "a"];
  (* Fold at one place *)
  !! Variable_basic.fold ~at:[cVarDef "r1"] [cVarDef "y"];
  (* Fold everywhere *)
  !! Variable_basic.fold [cVarDef "b"];

)
