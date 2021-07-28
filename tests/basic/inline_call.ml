open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function_basic.inline_call ~label:"body1" [cFun "f"];
  !! Function_basic.inline_call ~label:"body2" [cFun "g"];
  !! Function_basic.inline_call ~label:"body3" [cFun "h"];
  !! Function_basic.inline_call ~label:"body4" [cFun "m"];
)
