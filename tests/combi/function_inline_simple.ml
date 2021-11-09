open Optitrust
open Target

(* Fix this unit test *)


let _ = Run.script_cpp (fun _ ->
  
  !! Function.inline ~body_mark:"bodyf1" [cFun "f1"]

  !! Function.inline ~body_mark:"body" [cFun "g"];
  !! Function.inline ~body_mark:"body" [tIndex ~nb:2 0; cFun "f"];
  !! Function.inline ~body_mark:"bodyf" [cFun "f"];
  (* with naming of the arguments *)
  !! Trace.alternative (fun () ->
    !! Function.inline ~body_mark:"body" ~args:["v"] [cFun "f"];
    !!());
  (* inlining a function with if else branches *)
  (* inlining a function with one if branch *)
  !! Function.inline ~body_mark:"body" [cFun "h"]; 
  (* inlining a function of type void *)
  !! Function.inline ~body_mark:"body" [cFun "m"];
)
