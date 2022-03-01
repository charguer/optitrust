open Optitrust
open Target
open Ast
open Ast_fromto_AstC

(* Note: [address_elim] is not meant work in the presence of stack variables;
   thus [stackvar_elim] must be called first. *)

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

  !^ Trace.apply infix_elim;
  !^ Trace.apply stackvar_elim;
  !^ Trace.apply caddress_elim;  (* Press F6 on this line to see the encoding step; keep in mind that the output is not regular C code *) (* Press Alt+F6 to check the blank diff of the round-trip for caddress_elim+intro *)

  !! Trace.apply caddress_intro;
  !^ Trace.apply stackvar_intro;
  !^ Trace.apply infix_intro;
  !^ Trace.check_recover_original(); (* Press F6 on this line to see a blank diff if successful, or an error message if the full round-trip fails *)

)
