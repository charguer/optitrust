open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Variable_basic.inline ~at:[[sInstr "y = 9"]] [cVarDef "y"];
  !! Variable_basic.inline ~delete:true [cVarDef "a"];
  !! Variable_basic.inline ~at:[[sInstr "b = 9"]] [cVarDef "b"];
  !! Variable_basic.inline ~delete:true [cVarDef "v"];
)

