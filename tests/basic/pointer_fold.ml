open Optitrust
open Target

let _ = Run.script_cpp (fun _->
  (* Fold everywhere *)
  !! Variable_basic.fold ~as_reference:true [cVarDef "a"];
  (* Fold at one place *)
  !! Variable_basic.fold ~at:[cVarDef "r1"] ~as_reference:true [cVarDef "y"];
  (* For at one place, then another one *)
  !! Variable_basic.fold ~at:[cVarDef "r3"] ~as_reference:true [cVarDef "b"];
  !! Variable_basic.fold ~at:[sInstr "= 9"] ~as_reference:true [cVarDef "b"];
  !! Variable_basic.fold [cVarDef "v"];
)
