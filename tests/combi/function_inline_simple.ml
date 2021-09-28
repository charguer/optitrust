open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Function.inline ~label:"body" [cFun "f"];
  (* with naming of the arguments *)
  !! Trace.alternative (fun () ->
    !! Function.inline ~label:"body" ~args:["v"] [cFun "f"];
    !!());
  (* inlining a function with if else branches *)
  !! Function.inline ~label:"body" [cFun "g"];
  (* inlining a function with one if branch *)
  !! Function.inline ~label:"body" [cFun "h"]; 
  (* inlining a function of type void *)
  !! Function.inline ~label:"body" [cFun "m"];
)
