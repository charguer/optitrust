open Optitrust
open Target
open C_encoding

(* Note: [address_elim] is not meant work in the presence of stack variables;
   thus [stackvar_elim] must be called first. *)

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
    (*!! Show.(trm ~style:InternalAst)  (Trm.prepare_for_serialize (Trace.ast()));*)
    (*!! Xfile.serialize_to "foo.txt" (Trm.prepare_for_serialize(Trace.ast()));*)

  Scope.infer_var_ids ();

  !! Trace.apply infix_elim;
     Trace.apply stackvar_elim;
     Trace.apply caddress_elim;  (* Press F6 on this line to see the encoding step; keep in mind that the output is not regular C code *) (* Press Alt+F6 to check the blank diff of the round-trip for caddress_elim+intro *)

  !! Trace.apply caddress_intro;
     Trace.apply stackvar_intro;
     Trace.apply infix_intro;

  (* TODO: use let t = Trace.ast ... Trace.check_same_as t *)
  !! Trace.check_recover_original(); (* Press F6 on this line to see a blank diff if successful, or an error message if the full round-trip fails *)

)
