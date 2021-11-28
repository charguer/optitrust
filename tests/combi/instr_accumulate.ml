open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

  !! Instr.accumulate ~nb:6 [occFirst; cWriteVar "x"];
  !! Instr.accumulate ~nb:6 [occFirst; sInstr "result.x +="];
)
