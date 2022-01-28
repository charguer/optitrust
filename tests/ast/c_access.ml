open Optitrust
open Target
open Ast
open CRawAst_to_ast


let test_accesses () =
  let clang_ast = Clang.Ast.parse_file "c_access.cpp" in
  let raw_ast = Clang_to_astRawC.tr_ast clang_ast in
  let stackvar_ast = stackvar_elim raw_ast in
  Ast_check.check_transfo_is_identity ~test:"access" (fun t -> caddress_intro (caddress_elim t)) stackvar_ast

(* let _ = test_accesses () *)

let _ =
  Flags.dump_ast_details := true;
  Flags.bypass_cfeatures := true;
  Flags.use_new_encodings := true

let _ = Run.script_cpp (* ~filename:"c_big.cpp" ~prefix:"c_big" *) (fun () ->

  (* Note: address_elim might not work in the presence of stack variables *)
  (* NOTE: use the big-step shortcut for checking the full diff *)
  !^ Trace.apply stackvar_elim;
  !! Trace.apply caddress_elim; (* when viewing the AST at this stage, keep in mind that it is not valid C code *) (* TODO: I think we should have a flag in the file ast_to_rawC to display "set(p, v)"  instead of p = v ? also, we should not be printing arrows in that mode. (similar to what we had with ~decode:false in terms of printing certain primitive operations such as set and access) *)
   Trace.apply caddress_intro; (* TODO: the round trip is not correct at the moment *)
  !!  Trace.apply stackvar_intro;

  !^ Trace.check_recover_original(); (* ARTHUR: later add an optional value ?nb_steps to specify how many steps to go back, so it can be used around caddress_elim+intro. *)

)

(* ARTHUR: in case of crash, it would be nice to generate the _before file nevertheless *)
(* ARTHUR: find if clang_format has an option to tell that the user is mutating an argument. *)
