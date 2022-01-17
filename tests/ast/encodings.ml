open Optitrust
open Target


let test_encodings () = 
  let clang_ast = Clang.Ast.parse_file "c_ast.cpp" in 
  let raw_ast = Clang_to_astRawC.translate_ast clang_ast in 
  (* let ° be the usual function composition symbol *)
  (* test1: check if stackvar_intro ° stackvar_elim = id *)
  let raw_ast1 = stackvar_intro (stackvar_elim raw_ast) in 
  if raw_ast1 = raw_ast then Printf.printf "Test1 passed\n" else Printf.printf "Test1 failed\n";
   
  (* test2: check if caddress_intro ° caddress_elim = id *)
  let raw_ast2 = caddress_intro (caddress_elim raw_ast) in
  if raw_ast2 = raw_ast then Printf.printf "Test2 passed\n" else Printf.printf "Test2 failed\n";
  (* test3: check if decode ° encode = id *)
  let raw_ast3 = decode (encode raw_ast) in
  if raw_ast3 = raw_ast then Printf.printf "Test3 passed\n" else Printf.printf "Test3 failed\n";

let _ = test_encodings ()



