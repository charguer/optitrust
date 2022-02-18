open Optitrust
open Target

(* Fix this unit test *)


let _ = Run.script_cpp (fun _ ->
  
  (* !! Function.inline ~vars:(AddSuffix "${occ}") [occFirst;cFun "f?"]; *)
  !! Function.inline [cFun "vect_mul"];
  !! Function.inline [cFun "vect_add"];
  
  (* with naming of the arguments *)
  (* !! Trace.alternative (fun () ->
    (* TODO: Fix this error *)
    !! Function.inline  ~args:["v"] [nbMulti;cFunDef "main";cFun "f"];
    !!()); *)
  (* inlining a function with if else branches *)
  !! Function.inline [cFun "g"];
  (* inlining a function with one if branch *)
  !! Function.inline [cFun "h"]; 
  (* inlining a function of type void *)
  !! Function.inline [cFun "m"];
)
