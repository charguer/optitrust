open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Function.bind [cFun "f"];
)
