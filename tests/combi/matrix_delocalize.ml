open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    !! Matrix.delocalize ~var:"a" ~local_var:"x" ~init_zero:true ~acc_in_place:false  ~acc:"sum" ~dim:(var "N0") ~index:"i0" ~ops:(Delocalize_arith (Lit_int 0, Binop_add)) [cFor "i"];
    (* TODO: delocalize is missing the read operations (same bug as intro_mops) *)
    (* TODO: an alternative with ~last:true *)

   !! Trace.alternative (fun () ->
        !! Matrix.delocalize ~last:true ~var:"a" ~local_var:"x" ~init_zero:true ~acc_in_place:false  ~acc:"sum" ~dim:(var "N0") ~index:"i0" ~ops:(Delocalize_arith (Lit_int 0, Binop_add)) [cFor "i"];
        !!());

)

