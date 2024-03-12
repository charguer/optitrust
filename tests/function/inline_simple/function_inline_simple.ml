
open Optitrust
open Target

(* TODO: let _ = Flags.check_validity := true *)

(* Fix this unit test *)
let _ = Run.script_cpp (fun _ ->

  (* !! Function.inline ~vars:(AddSuffix "${occ}") [occFirst;cFun "f?"]; *)
  !! Function.inline [nbMulti; cFun "vect_mul"];
  !! Function.inline [nbMulti; cFun "vect_add"];
  !! Function.inline [nbMulti; cFun "vect_op"];
  (* TODO
   vect r;
  if (true) {
    {
      r = v;
      goto exit_body;
    }
  }
const vect w = r
*)

  !! Function.inline ~keep_res:true [nbMulti; cFun "vect_op2"];
  (* LATER: have an option to function inline for keeping "res" and not eliminating it ==> deactivate the final step
    vect res = {0,0};
    res.x = 1;
    vect w2 = res;

  *)

  (* inlining a function with single return *)
  !! Function.inline ~vars:(AddSuffix "${occ}") [nbMulti; cFun "f"];
  (* inlining a function with if else branches *)
  !! Function.inline [cTopFunDef "main"; cFun "g"];
  !! Function.inline ~resname:"r" [cTopFunDef "test_const_ret"; cFun "g"];

  (* inlining a function with one if branch *)
  !! Function.inline [nbMulti; cFun "h"];
  (* inlining a function of type void *)
  !! Function.inline [nbMulti; cFun "m"];

  (* with naming of the arguments *)
  !! Trace.restore_original();
  !! Function.inline ~vars:(Variable.Rename.add_suffix "2") ~args:["v"] [nbMulti;cFunDef "main";cFun "f"];

)
