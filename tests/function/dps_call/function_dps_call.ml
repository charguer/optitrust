open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Function_basic.dps_call [cCall "f"];
  !! Function_basic.dps_call [cCall "g"];
  !! Function_basic.dps_call ~dps:"my_h" [cCall "h"];

)
