open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  let tf = cTopFunDef "test_fun" in
  !! Function_basic.inline ~body_mark:"bodyf" [tf;cFun "f"];
  !! Function_basic.inline [tf;cFun "g"];
  !! Function_basic.inline ~body_mark:"bodyh" [tf;cFun "h"];
  !! Function_basic.inline [tf;cFun "m"];
  !! Function_basic.inline ~body_mark:"bodyk" ~subst_mark:"substk" [tf;cFun "k"];

  let tc = cTopFunDef "test_class_method" in

   (* Note: before inline any class method, all the members of the class should be made public! *)
  !! Record_basic.make_all_memebers_public [cTypDef "Test_method_inline"];

  !! Function_basic.inline ~body_mark:"bodyf1" [tc;cFun "f1"];

  !! Function_basic.inline ~body_mark:"bodyf1" [tc;cFun "f"];

  !! Function_basic.inline [tc;cFun "g"];
  !! Function_basic.inline ~body_mark:"bodyh" [tc;cFun "h"];
  !! Function_basic.inline [tc;cFun "m"];
  !! Function_basic.inline ~body_mark:"bodyk" [tc;cFun "k"];

  !! Trace.failure_expected (fun () ->
    Function_basic.inline [cTopFunDef "test_nameclash"; cFun "f"]);
)
