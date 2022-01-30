open Optitrust
open Target
open Ast
open CRawAst_to_ast

(* TODO: this test seems superseeded by others, let's remove it *)

let test_const () =
  let clang_ast = Clang.Ast.parse_file "const.cpp" in
  let raw_ast = Clang_to_astRawC.tr_ast clang_ast in
  let stackvar_ast = stackvar_elim raw_ast in
  Ast_check.check_transfo_is_identity ~test:"access" (fun t -> caddress_intro (caddress_elim t)) stackvar_ast

let _ = test_const ()

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp ~raw_ast:true (fun () ->
  (* Note: address_elim might not work in the presence of stack variables *)
  !! Trace.apply stackvar_elim;
     Trace.apply caddress_elim; (* Press F6 on this line *)
     Trace.apply caddress_intro;
     Trace.apply stackvar_intro;

)
