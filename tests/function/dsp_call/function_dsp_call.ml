open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Function_basic.dsp_call [cCall "f"];
  !! Function_basic.dsp_call [cCall "g"];
  !! Function_basic.dsp_call ~dsp:"my_h" [cCall "h"];

)
