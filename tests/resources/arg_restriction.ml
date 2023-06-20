open Optitrust
open Target

(*let _ = Flags.bypass_cfeatures_decoding := true*)

let _ = Run.script_cpp (fun () ->
    show_ast ();
    show_res ();
)
