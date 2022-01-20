open Optitrust
open Target
open Ast
open CRawAst_to_ast

(* TODO: rename this test file to c_cfeatures.ml to match the name of the function tested  *)

let test_elim_intro () =
  let clang_ast = Clang.Ast.parse_file "c_elimintro.cpp" in
  let raw_ast = Clang_to_astRawC.translate_ast clang_ast in
  Ast_check.check_transfo_is_identity ~test:"elim_intro" (fun t -> cfeatures_intro (cfeatures_elim t)) raw_ast

let _ = test_elim_intro ()


let _ = Flags.dump_ast_details := true

(* TODO: try to run the test using ~filename:"c_big.cpp";
   at the very least, you need to deal with variable names coming from libraries,
   e.g. "malloc", instead of raising an error, it should be assumed a var immutable. *)

let _ = Run.script_cpp ~raw_ast:true (fun () ->

  !^ Trace.apply cfeatures_elim;
  !! Trace.apply cfeatures_intro;
  (* use f6 to see one step, alt-f6 to check that the round trip is the identity *)
)
