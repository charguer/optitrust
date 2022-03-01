open Optitrust
open Target
open Ast
open Ast_fromto_AstC


let _ =
  Flags.dump_ast_details := true;
  Flags.bypass_cfeatures := true

(* Option to choose the size of the test *)
let filename =
  match 2 with
  | 0 -> "c_debug.cpp"
  | 1 -> "c_mid.cpp"
  | _ -> "c_big.cpp"

let _ = Run.script_cpp ~filename (fun () ->

  (* If this test fails, see c_access.ml or c_stackvar.ml for debugging *)
  !^ Trace.apply cfeatures_elim;
  !! Trace.apply cfeatures_intro;
  !^ Trace.check_recover_original();
)

(* ARTHUR: in case of crash, it would be nice to generate the _before file nevertheless *)
(* ARTHUR: find if clang_format has an option to tell that the user is mutating an argument. *)


(* FOR DEBUG
let test_accesses =
  let clang_ast = Clang.Ast.parse_file "c_access.cpp" in
  let raw_ast = Clang_to_astRawC.tr_ast clang_ast in
  let stackvar_ast = stackvar_elim raw_ast in
  Ast_check.check_transfo_is_identity ~test:"access" (fun t -> caddress_intro (caddress_elim t)) stackvar_ast
*)
