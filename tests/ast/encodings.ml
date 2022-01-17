open Optitrust
open Target
open CRawAst_to_ast

let test_encodings () = 
  let clang_ast = Clang.Ast.parse_file "c_ast.cpp" in 
  let raw_ast = Clang_to_astRawC.translate_ast clang_ast in 
  (* let ° be the usual function composition symbol *)
  (* test1: check if stackvar_intro ° stackvar_elim = id *)
  let raw_ast1 = stackvar_intro (stackvar_elim raw_ast) in 

  if Ast_to_text.ast_to_string raw_ast1 = Ast_to_text.ast_to_string raw_ast then Printf.printf "Test1 passed\n" else Printf.printf "Test1 failed\n";
   
  (* test2: check if caddress_intro ° caddress_elim = id *)
  let raw_ast2 = caddress_intro false (caddress_elim false raw_ast) in
  let out1 = open_out "before_enc.ast" in 
  let out2 = open_out "after_enc.ast" in 
  Ast_to_text.print_ast out1 raw_ast;
  Ast_to_text.print_ast out2 raw_ast2;
  close_out out1;
  close_out out2;
  if Ast_to_text.ast_to_string raw_ast2 = Ast_to_text.ast_to_string raw_ast then Printf.printf "Test2 passed\n" else Printf.printf "Test2 failed\n"
  (* test3: check if decode ° encode = id *)
  (* let raw_ast3 = decode (encode raw_ast) in *)
  (* if raw_ast3 = raw_ast then Printf.printf "Test3 passed\n" else Printf.printf "Test3 failed\n" *)

let _ = test_encodings ()
