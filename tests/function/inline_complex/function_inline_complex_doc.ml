open Optitrust
open Target


let _ = Run.script_cpp (fun () ->

  !! Function.inline [cFun "g"];

)
