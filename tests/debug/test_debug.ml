open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->


  (* !! Function.inline [nbMulti; cFunDef "test_const_ret"; cFun "g"]; *)
  !! Function.inline [nbMulti; cFunDef "test_const_arg"; cFun "g"];
)

