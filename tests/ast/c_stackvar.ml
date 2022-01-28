open Optitrust
open Target
open Ast
open Ast_to_rawC
open CRawAst_to_ast

let _ =
  Flags.dump_ast_details := true;
  Flags.bypass_cfeatures := true;
  Flags.use_new_encodings := true

(* Option to choose the size of the test *)
let perform_big_test = true

let filename =
  if perform_big_test then "c_big.cpp" else "c_stackvar.ml"

let _ = Run.script_cpp ~filename ~prefix:"c_stackvar" (fun () ->
  !^ Trace.apply stackvar_elim;   (* Press F6 on this line to see the encoding *) (* Press Alt+F6 to check the blank diff of the round-trip *)
  !! Trace.apply stackvar_intro; (* Press F6 on this line to see the decoding *)
  !^ Trace.check_recover_original(); (* Press F6 on this line to see a blank diff if successful, or an error message if round-trip fails *)
 )

(* FOR DEBUG
let test_stackvar () =
  let clang_ast = Clang.Ast.parse_file "c_stackvar.cpp" in
  let raw_ast = Clang_to_astRawC.tr_ast clang_ast in
  Ast_check.check_transfo_is_identity ~test:"Stack variables" (fun t -> stackvar_intro (stackvar_elim t)) raw_ast
*)