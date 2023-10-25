open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Instr.accumulate_targets [cWriteVar "x"];
  !! Instr.accumulate_targets [sInstr "result.x +="];
)
