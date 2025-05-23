open Optitrust
open Target
open C_encoding


(* let _ = Flags.dump_ast_details := true; *)
let _ = Flags.bypass_cfeatures := true
let _ = Flags.print_optitrust_syntax := true

(* Option to choose the size of the test *)
let filename =
  match 2 with
  | 0 -> "c_debug.cpp"
  | 1 -> "c_mid.cpp"
  | _ -> "c_big.cpp" (* DEPRECATED *)

let _ = Run.script_cpp ~filename (fun () ->

  (* If this test fails, see c_access.ml or c_stackvar.ml for debugging *)
  bigstep "round-trip";
  !! Trace.apply decode_from_c;
  !! Trace.apply (encode_to_c (default_style ()));
  bigstep "check";
  !! Trace.check_recover_original();
)

(* ARTHUR: in case of crash, it would be nice to generate the _before file nevertheless *)
(* ARTHUR: find if clang_format has an option to tell that the user is mutating an argument. *)


(* FOR DEBUG
let test_accesses =
  let clang_ast = Clang.Ast.parse_file "c_access.cpp" in
  let raw_ast = Clang_to_astRawC.tr_ast clang_ast in
  let stackvar_ast = decode_stackvar raw_ast in
  Ast_check.check_transfo_is_identity ~test:"access" (fun t -> encode_caddress (decode_caddress t)) stackvar_ast
*)
