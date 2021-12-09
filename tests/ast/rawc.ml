open Optitrust
open Target


let test_raw_ast () = 
  let clang_ast = Clang.Ast.parse_file "c_ast.cpp" in
  let raw_ast = Clang_to_astRawC.translate_ast clang_ast in 
  let code = Ast_to_rawC.ast_to_string raw_ast in
  let out_file = "c_ast_out.cpp" in
  Xfile.put_contents out_file code



let _ = test_raw_ast ()
