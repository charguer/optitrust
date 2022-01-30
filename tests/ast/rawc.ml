open Optitrust
open Target

(* This test if for testing only the roudntrip of parsing+printing, without encodings *)

(* TODO: the test should execute a command to check that the diff is empty, e.g.
      let r = Sys.command (diff ... ) in
      r = 0
  *)

  (* TODO: this file should be renamed c_raw.ml
     it should use as input file  c_raw.cpp,
     which could be an alias for c_ast.cpp (to avoid duplicating the file;

     then the test can be added to the Makefile

     TODO: not sure it compiles
   *)

let test_raw_ast =
  let clang_ast = Clang.Ast.parse_file "c_ast.cpp" in
  let raw_ast = Clang_to_astRawC.translate_ast clang_ast in
  Ast_to_rawC.ast_to_file "rawc_out.cpp" raw_ast

