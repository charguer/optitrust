open Optitrust
open Target

(*let _ = Flags.bypass_cfeatures := true*)

let _ = Run.script_cpp (fun () ->
    Resources.show_ast ();
    Resources.show ();
)
