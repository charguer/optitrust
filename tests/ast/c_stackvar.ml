open Optitrust
open Ast_fromto_AstC
open Target

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
  Trace.apply C_scope.infer_var_ids;

  (* TODO  let t = Trace.ast() in *)
  !! Trace.apply stackvar_elim;   (* Press F6 on this line to see the encoding *) (* Press Alt+F6 to check the blank diff of the round-trip *)
  !! Trace.apply stackvar_intro; (* Press F6 on this line to see the decoding *)
  !! Trace.check_recover_original(); (* Press F6 on this line to see a blank diff if successful, or an error message if round-trip fails *)
 )
