open Optitrust
open Target 

let _ = Run.script_cpp (fun () -> 

  !! Apac.sync_with_taskwait [cFunDef "f"];

)
