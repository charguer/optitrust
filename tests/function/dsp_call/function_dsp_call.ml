open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Function_basic.dsp_call [cFun "f"];
  !! Function_basic.dsp_call [cFun "g"];
  !! Function_basic.dsp_call ~dsp:"my_h" [cFun "h"];

)
