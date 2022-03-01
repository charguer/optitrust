open Optitrust
open Target

(* Fix this unit test *)


let _ = Run.script_cpp (fun _ ->

  (* inlining a function with single return *)
  !! Function.inline [nbMulti; cFun "f"];
  (* inlining a function with if else branches *)
  !! Function.inline [nbMulti; cFun "g"];
  (* inlining a function with one if branch *)
  !! Function.inline [nbMulti; cFun "h"];
  (* inlining a function of type void *)
  !! Function.inline [nbMulti; cFun "m"];


  (* !! Function.inline ~vars:(AddSuffix "${occ}") [occFirst;cFun "f?"]; *)
  !! Function.inline [cFun "vect_mul"];
  !! Function.inline [cFun "vect_add"];

  (* with naming of the arguments *)
  (* !! Trace.alternative (fun () ->
    (* TODO: Fix this error *)
    !! Function.inline  ~args:["v"] [nbMulti;cFunDef "main";cFun "f"];
    !!()); *)

  (* TODO: add tests with ~args  and with ~vars *)

)
