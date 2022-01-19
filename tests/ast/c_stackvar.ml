open Optitrust
open Target
open Ast
open Ast_to_rawC
open CRawAst_to_ast


let test_stackvar () =
  let clang_ast = Clang.Ast.parse_file "c_stackvar.cpp" in
  let raw_ast = Clang_to_astRawC.translate_ast clang_ast in
  Ast_check.check_transfo_is_identity ~test:"Stack variables" (fun t -> stackvar_intro (stackvar_elim t)) raw_ast

let _ = test_stackvar ()

let _ = Run.script_cpp ~raw_ast:true (fun () ->
  !! Trace.apply stackvar_elim;
  !! Trace.apply stackvar_intro;
 )
