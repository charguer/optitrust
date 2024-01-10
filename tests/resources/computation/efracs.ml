open Optitrust

let _ = Run.script_cpp (fun () ->
  Show.ast ();
  Resources.show ();
)
