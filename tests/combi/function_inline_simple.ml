open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.inline_call ~label:"body" [cFun "f"];
  (* with naming of the arguments *)
  !! Trace.alternative (fun () ->
    !! Function.inline_call ~label:"body" ~args:["v"] [cFun "f"];
    !!());
  !! Function.inline_call ~label:"body" [cFun "g"];
  !! Function.inline_call ~label:"body" [cFun "h"];
  !! Function.inline_call ~label:"body" [cFun "m"];
)
