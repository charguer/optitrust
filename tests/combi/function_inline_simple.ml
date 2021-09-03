open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.inline_call ~label:"body" [cFun "f"];
  (* with naming of the arguments *)
  !! Trace.alternative (fun () ->
    !! Function.inline_call ~label:"body" ~args:["v"] [cFun "f"];
    !!());
  !! Function.inline_call ~label:"body" ~_no_control_structures:false [cFun "g"];
  !! Function.inline_call ~label:"body" ~_no_control_structures:false [cFun "h"];
  !! Function.inline_call ~label:"body" [cFun "m"];
)
