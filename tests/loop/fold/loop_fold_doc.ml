open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Loop.fold ~index:"k" 4 [sInstr "a += 0"];

)
