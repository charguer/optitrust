open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Specialize.function_arg "f1" [false] [cFun "f"];

)
