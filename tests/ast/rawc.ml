open Optitrust
open Target


let test_raw_ast () = 
  let clang_ast = Clang.Ast.parse_file "c_ast.cpp" in
  let raw_ast = Clang_to_astRawC.translate_ast clang_ast in 
  let out_file = open_out "c_ast_out.cpp" in
  Ast_to_rawC.ast_to_doc out_file raw_ast;
  output_string out_file "\n";
  close_out out_file

let _ = test_raw_ast ()
