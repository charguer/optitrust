open Optitrust
open Target

(*let _ = Flags.resource_errors_as_warnings := true*)

let _ = Run.script_cpp (fun () ->
    show_ast ();
    Resources.show ();
)
