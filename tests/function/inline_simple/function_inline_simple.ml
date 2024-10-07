
open Optitrust
open Target

(* TODO: let _ = Flags.check_validity := true *)

(* Fix this unit test *)
let _ = Run.script_cpp (fun _ ->

  (* !! Function.inline ~vars:(AddSuffix "${occ}") [occFirst;cCall "f?"]; *)
  !! Function.inline [nbMulti; cCall "vect_mul"];
  !! Function.inline [nbMulti; cCall "vect_add"];
  !! Function.inline [nbMulti; cCall "vect_op"];

  !! Function.inline [nbMulti; cCall "vect_op2"];

  (* inlining a function with single return *)
  !! Function.inline ~vars:(AddSuffix "${occ}") [nbMulti; cCall "f"];
  (* inlining a function with if else branches *)
  !! Function.inline [cTopFunDef "main"; cCall "g"];
  !! Function.inline [cTopFunDef "test_const_ret"; cCall "g"];

  (* inlining a function with one if branch *)
  !! Function.inline [nbMulti; cCall "h"];
  (* inlining a function of type void *)
  !! Function.inline [nbMulti; cCall "m"];

  (* with naming of the arguments *)
  !! Trace.restore_original();
  !! Function.inline ~vars:(Variable.Rename.add_suffix "2") ~args:["v"] [nbMulti;cFunDef "main";cCall "f"];

)
