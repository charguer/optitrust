open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  show_ast ();
  Resources.show ();
)