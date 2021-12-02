open Optitrust
open Target

(* Fix this unit test *)


let _ = Run.script_cpp (fun _ ->
  

  (* !! Function.inline ~vars:(AddSuffix "${occ}") [occFirst;cFun "f?"]; *)
  !! Function.inline [cFun "vect_mul"];
  !! Function.inline [cFun "vect_add"];
  
  (* with naming of the arguments *)
  !! Trace.alternative (fun () ->
    !! Function.inline  ~args:["v"] [cFun "f"];
    !!());
  (* inlining a function with if else branches *)
  (* inlining a function with one if branch *)
  !! Function.inline [cFun "h"]; 
  (* inlining a function of type void *)
  !! Function.inline [cFun "m"];
)
