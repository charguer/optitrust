open Optitrust
open Target

let _ = Run.script_cpp (fun () -> 

  !! Align.alloc (lit "64") [cFun "MALLOC1"];
  !! Align.alloc (lit "64") [cFun "MALLOC3"];
)