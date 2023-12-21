open Optitrust

let _ = Flags.resource_errors_as_warnings := false

let _ = Run.script_cpp (fun () ->
  Show.ast ();
  Resources.show ();
)
