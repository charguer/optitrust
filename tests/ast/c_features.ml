open Optitrust
open Target
open Ast
open Ast_fromto_AstC


let _ =
  Flags.dump_ast_details := true;
  Flags.bypass_cfeatures := true;
  Flags.use_new_encodings := true


let filename = "c_big.cpp"


let _ = Run.script_cpp ~filename ~prefix:"c_features" (fun () ->

  (* If this test fails, see c_access.ml or c_stackvar.ml for debugging *)
  !^ Trace.apply cfeatures_elim;
  !! Trace.apply cfeatures_intro;
  !^ Trace.check_recover_original();
)


(* FOR DEBUG
let test_features () =
  let clang_ast = Clang.Ast.parse_file "c_features.cpp" in
  let raw_ast = Clang_to_astRawC.tr_ast clang_ast in
  Ast_check.check_transfo_is_identity ~test:"elim_intro" (fun t -> cfeatures_intro (cfeatures_elim t)) raw_ast
*)