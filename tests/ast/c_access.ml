open Optitrust
open Target
open Ast
open Ast_to_rawC
open CRawAst_to_ast



(* Note: .cpp --> clang --> clang_to_rawast -[t1]-> stackvar_elim -> [t2] -> *)

let test_accesses () =
  let clang_ast = Clang.Ast.parse_file "c_access.cpp" in
  let raw_ast = Clang_to_astRawC.translate_ast clang_ast in
  let stackvar_ast = stackvar_elim raw_ast in 
  Ast_check.check_transfo_is_identity ~test:"access" (fun t -> caddress_intro (caddress_elim t)) stackvar_ast

let _ = test_accesses ()


let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp ~raw_ast:true (fun () ->
  (* Note: address_elim might not work in the presence of stack variables *)
  !! Trace.apply stackvar_elim;
  !! Trace.apply caddress_elim;
     Trace.apply caddress_intro;
  !! Trace.apply stackvar_intro;
)

(* ARTHUR: in case of crash, it would be nice to generate the _before file nevertheless *)
(* ARTHUR: find if clang_format has an option to tell that the user is mutating an argument. *)