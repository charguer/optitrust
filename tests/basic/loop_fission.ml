open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Loop_basic.fission [tAfter; sInstr "t[i] +="];
  !! Loop_basic.fission [tAfter; cVarDef "b"];
  !! Loop_basic.fission [tBefore; sInstr "t[i] +="];
)
