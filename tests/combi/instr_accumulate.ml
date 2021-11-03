open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

  !! Instr.accumulate ~nb:6 [tIndex 0; cWriteVar "x"];
  !! Instr.accumulate ~nb:6 [tIndex 0; sInstr "result.x +="];
)
