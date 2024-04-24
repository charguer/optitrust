open Optitrust
open Prelude
open Ast_fromto_AstC

(* let _ = Flags.dump_ast_details := true; *)
let _ = Flags.bypass_cfeatures := true
let _ = Flags.print_optitrust_syntax := true


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
