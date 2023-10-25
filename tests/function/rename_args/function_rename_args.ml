open Optitrust
open Target
open Prelude


let _ = Run.script_cpp (fun () ->

  !! Function_basic.rename_args [] [cFunDef "test_no_args"];

  !! Function_basic.rename_args [""] [cFunDef "test_one_arg"];
  !! Function_basic.rename_args ["x1"] [cFunDef "test_one_arg"];

  !! Function_basic.rename_args ["x1"; ""] [cFunDef "test_two_args"];
  !! Function_basic.rename_args [""; "y1"] [cFunDef "test_two_args"];
  !! Function_basic.rename_args ["x2"; "y2"] [cFunDef "test_two_args"];

  !! Function_basic.rename_args ["x1"] [cFunDef "test_method_args"];
)