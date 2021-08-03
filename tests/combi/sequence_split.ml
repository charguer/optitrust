open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->
  !! Sequence_basic.split [tAfter; sInstr "x += y"];
)