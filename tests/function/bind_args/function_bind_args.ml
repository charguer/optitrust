open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Function.bind_args ["a";"";"b";""] [cTopFunDef "main"; cFun "g"];
  (* It also works if the function is nested in a deeper context *)
  !! Function.bind_args ["a";"";"b";"c"] [cTopFunDef "main2"; cFun "g"];
  (* Note: the transformation does nothing if the list of args is undefined *)
  !! Trace.restore_original();
  !! Function.bind_args [] [cTopFunDef "main2"; cFun "g"];
  !! Trace.restore_original();
  !! Trace.failure_expected (fun _e -> true) (fun _ ->
      Function.bind_args ["a"] [cTopFunDef "main2"; cFun "g"]);

)
