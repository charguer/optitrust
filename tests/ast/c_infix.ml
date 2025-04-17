open Optitrust
open C_encoding
open Target

(* Note: [address_elim] is not meant work in the presence of stack variables;
   thus [decode_stackvar] must be called first. *)

(* let _ = Flags.dump_ast_details := true; *)
let _ = Flags.bypass_cfeatures := true
let _ = Flags.print_optitrust_syntax := true


(* Option to choose the size of the test *)

let filename =
  match 2 with
  | 0 -> "c_debug.cpp"
  | 1 -> "c_mid.cpp"
  | _ -> "c_big.cpp"

let _ = Run.script_cpp ~filename (fun () ->

  !! Trace.apply decode_infix;
  !! Trace.apply encode_infix;
  !! Trace.check_recover_original(); (* Press F6 on this line to see a blank diff if successful, or an error message if the full round-trip fails *)
)
