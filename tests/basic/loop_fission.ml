open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Loop.fission [tAfter; sInstr "t[i] += a"];
  !! Loop.fission [tAfter; cVarDef "b"];
  !! Loop.fission [tBefore; sInstrRegexp ~substr:true "t\[.\]"];
)
