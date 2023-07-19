open Optitrust
open Target
open Syntax
open Ast_fromto_AstC

(* let _ = Flags.set_dump_clang_ast() *)

let _ =
  Flags.dump_ast_details := true;
  Flags.bypass_cfeatures := false


(* Option to choose the size of the test *)
let filename =
  match 2 with
  | 0 -> "cpp_debug.cpp"
  | _ -> "cpp_vector.cpp"

let _ = Run.script_cpp ~filename (fun () ->

  (* If this test fails, see c_access.ml or c_stackvar.ml for debugging *)

  bigstep "round-trip";
  !! Trace.apply cfeatures_elim;
  !! Trace.apply cfeatures_intro;
  bigstep "check";
  !! Trace.check_recover_original();
)
