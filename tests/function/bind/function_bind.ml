open Optitrust
open Target

let _ = Run.script_cpp ~parser:CParsers.clang (fun _ ->
  !! Function.bind ~fresh_name:"r" ~args:["a";"";"b";""] [cFun "g"];

  (* default is to not name any of the arguments *)
  !! Trace.restore_original();
  !! Function.bind ~fresh_name:"r" [cFun "g"];

  (* default name is "res" for the result *)
  !! Trace.restore_original();
  !! Function.bind [cFun "g"];

)
