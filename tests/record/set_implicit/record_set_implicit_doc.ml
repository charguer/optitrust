open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Record.set_implicit [sInstr "v.x = w.x"];

)
