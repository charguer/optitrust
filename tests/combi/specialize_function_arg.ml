open Optitrust
open Target


let _ = Run.script_cpp (fun _ -> 


  !! Specialize.function_arg "f1" [true; false] [occFirst; cFun "f"];

  !! Specialize.function_arg "f2" [false; true] [occFirst; cFun "f"];

)