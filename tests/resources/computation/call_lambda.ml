open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
    Resources.show_ast ();
    Resources.show ();
)
