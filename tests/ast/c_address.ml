open Optitrust
open Target
open C_encoding

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
    (*!! Show.(trm ~style:InternalAst)  (Trm.prepare_for_serialize (Trace.ast()));*)
    (*!! Xfile.serialize_to "foo.txt" (Trm.prepare_for_serialize(Trace.ast()));*)

  Scope.infer_var_ids ();

  !! Trace.apply decode_infix;
     Trace.apply decode_stackvar;
     Trace.apply decode_caddress;  (* Press F6 on this line to see the encoding step; keep in mind that the output is not regular C code *) (* Press Alt+F6 to check the blank diff of the round-trip for decode_caddress+intro *)

  !! Trace.apply encode_caddress;
     Trace.apply encode_stackvar;
     Trace.apply encode_infix;

  (* TODO: use let t = Trace.ast ... Trace.check_same_as t *)
  !! Trace.check_recover_original(); (* Press F6 on this line to see a blank diff if successful, or an error message if the full round-trip fails *)

)
