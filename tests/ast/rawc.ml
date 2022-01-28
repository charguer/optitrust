open Optitrust
open Target


let test_raw_ast () =
  let clang_ast = Clang.Ast.parse_file "c_ast.cpp" in
  let raw_ast = Clang_to_astRawC.translate_ast clang_ast in
  Ast_to_rawC.ast_to_file "rawc_out.cpp" raw_ast

let _ = test_raw_ast ()
