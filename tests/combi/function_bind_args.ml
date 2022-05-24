open Optitrust
open Target

let _ = Run.script_cpp ~parser:Parsers.Clang (fun _ ->

  !! Function.bind_args ["a";"";"b";""] [cTopFunDef "main"; cFun "g"];
  (* It also works if the function is nested in a deeper context *)
  !! Function.bind_args ["a";"";"b";"c"] [cTopFunDef "main2"; cFun "g"];
  (* Note: the transformation does nothing if the list of args is undefined *)
  !! Trace.alternative (fun _ ->
    !! Function.bind_args [] [cTopFunDef "main2"; cFun "g"];
    !! ());
  !! Tools.failure_expected (fun _ ->
    Function.bind_args ["a"] [cTopFunDef "main2"; cFun "g"]);
)
