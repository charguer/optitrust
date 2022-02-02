open Optitrust
open Target
open Ast
open Ast_fromto_AstC

(* Note: [address_elim] is not meant work in the presence of stack variables;
   thus [stackvar_elim] must be called first. *)

let _ =
  Flags.dump_ast_details := true;
  Flags.bypass_cfeatures := true;
  Flags.use_new_encodings := true


(* Option to choose the size of the test *)

let filename =
  match 2 with
  | 0 -> "c_debug.cpp"
  | 1 -> "c_access.cpp"
  | _ -> "c_big.cpp"


let _ = Run.script_cpp ~filename ~prefix:"c_access" (fun () ->

  !^ Trace.apply compound_assign_elim;
  !^ Trace.apply stackvar_elim;
  !^ Trace.apply caddress_elim;  (* Press F6 on this line to see the encoding step; keep in mind that the output is not regular C code *) (* Press Alt+F6 to check the blank diff of the round-trip for caddress_elim+intro *)

  !! Trace.apply caddress_intro;
  !^ Trace.apply stackvar_intro;
  !^ Trace.apply compound_assign_intro;
  !^ Trace.check_recover_original(); (* Press F6 on this line to see a blank diff if successful, or an error message if the full round-trip fails *)

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
