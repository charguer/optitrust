open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Instr.move ~dest:[tBefore; cIf()] [cVarDef "c"];
  )
