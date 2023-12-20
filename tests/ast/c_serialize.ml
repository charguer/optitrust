open Optitrust
open Prelude
open Ast_fromto_AstC

(* This test is used to check if we can serialize the current ast
  and recover it from its serialized version*)
let test_serialize =
  let clang_ast = Clang.Ast.parse_file "c_mid.cpp" in
  let raw_ast = Clang_to_astRawC.tr_ast clang_ast in
  let out_file = open_out "serialize.ser" in
  Marshal.to_channel out_file raw_ast [];
  close_out out_file;
  let in_file = open_in "serialize.ser" in
  let raw_ast1 = Marshal.from_channel in_file in
  if Ast_to_text.ast_to_string raw_ast = Ast_to_text.ast_to_string raw_ast1 then Printf.printf "Success\n" else Printf.printf "Failure\n"


let _ = Run.script_cpp (fun _ ->

  !!();
)
