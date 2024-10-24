open Optitrust
open Target

(* let _ = Flags.check_validity := true *)

let _ = Run.script_cpp (fun _ ->

  let tf = cTopFunDef "test_fun" in
  !! Function_basic.inline ~body_mark:"bodyf" [tf;cCall "f"];
  !! Function_basic.inline [tf;cCall "g"];
  !! Function_basic.inline ~body_mark:"bodyh" [tf;cCall "h"];
  !! Function_basic.inline [tf;cCall "m"];
  !! Function_basic.inline ~body_mark:"bodyk" ~subst_mark:"substk" [tf;cCall "k"];

  (* FIXME:
  let tc = cTopFunDef "test_class_method" in

   (* Note: before inline any class method, all the members of the class should be made public! *)
  !! Record_basic.make_all_memebers_public [cTypDef "Test_method_inline"];

  !! Function_basic.inline ~body_mark:"bodyf1" [tc;cCall "f1"];
  !! Function_basic.inline ~body_mark:"bodyf1" [tc;cCall "f"];

  !! Function_basic.inline [tc;cCall "g"];
  !! Function_basic.inline ~body_mark:"bodyh" [tc;cCall "h"];
  !! Function_basic.inline [tc;cCall "m"];
  !! Function_basic.inline ~body_mark:"bodyk" [tc;cCall "k"];
  *)

  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Function_basic.inline [cTopFunDef "test_nameclash"; cCall "f"]);

  !! Function.inline ~recurse:true [multi cCall ["recurse3"; "recurse2"]];
)
