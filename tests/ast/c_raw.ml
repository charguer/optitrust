open Optitrust
open Target

(* This test if for testing only the roudntrip of parsing+printing, without encodings *)

(* LATER: the test should execute a command to check that the diff is empty, e.g.
      let r = Sys.command (diff ... ) in
      r = 0
  *)

let test_raw_ast () =

  let clang_ast = Clang.Ast.parse_file "c_raw.cpp" in
  let raw_ast = Clang_to_astRawC.tr_ast clang_ast in
  AstC_to_c.(ast_to_file style()) "c_raw_out.cpp" raw_ast


let _ = test_raw_ast()
