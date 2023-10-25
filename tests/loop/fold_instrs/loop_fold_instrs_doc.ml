open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  !! Loop.fold_instrs ~index:"k" [cWriteVar "a"];

)
