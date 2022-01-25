open Optitrust
open Target
open Ast
open CRawAst_to_ast

let test_features () =
  let clang_ast = Clang.Ast.parse_file "c_features.cpp" in
  let raw_ast = Clang_to_astRawC.tr_ast clang_ast in
  Ast_check.check_transfo_is_identity ~test:"elim_intro" (fun t -> cfeatures_intro (cfeatures_elim t)) raw_ast

let _ = test_features ()


let _ = Flags.dump_ast_details := true


let _ = Run.script_cpp (* ~filename:"c_big.cpp" ~prefix:"c_big" *) ~raw_ast:true (fun () ->

  !^ Trace.apply cfeatures_elim;
     Trace.apply cfeatures_intro;
  (* use f6 to see one step, alt-f6 to check that the round trip is the identity *)
)
