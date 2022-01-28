open Optitrust
open Target
open Ast
open Ast_to_rawC
open CRawAst_to_ast


let test_stackvar () =
  let clang_ast = Clang.Ast.parse_file "c_stackvar.cpp" in
  let raw_ast = Clang_to_astRawC.tr_ast clang_ast in
  Ast_check.check_transfo_is_identity ~test:"Stack variables" (fun t -> stackvar_intro (stackvar_elim t)) raw_ast

let _ =
  Flags.dump_ast_details := true;
  Flags.bypass_cfeatures := true;
  Flags.use_new_encodings := true

let _ = Run.script_cpp ~filename:"c_big.cpp" ~prefix:"c_stackvar" (fun () ->
  !! Trace.apply stackvar_elim;  (* Press F6 on this line, with !! in front of the next line *)
     Trace.apply stackvar_intro;
 )
