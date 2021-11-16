open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Instr.(gather ~dest:GatherAtFirst) [cVarDef ""];
)
