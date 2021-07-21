open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Loop.fission [sInstr "u[i] += i"];
)