open Optitrust
open Target


let _ = Run.script_cpp (  fun _->
  !! Variable_basic.fold ~at:[[cVarDef "r1"]] [cVarDef "y"];
  !! Variable_basic.fold [cVarDef "a"];
  !! Variable_basic.fold ~at:[[cVarDef "r3"]] [cVarDef "b"];
  !! Variable_basic.fold [cVarDef "v"];
)
