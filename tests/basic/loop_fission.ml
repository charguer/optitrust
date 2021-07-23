open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Loop_basic.fission [tAfter; sInstr "t[i] += a"];
  !! Loop_basic.fission [tAfter; cVarDef "b"];
  !! Loop_basic.fission [tBefore; sInstrRegexp ~substr:true "t\[.\]"];
)
