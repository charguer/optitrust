open Optitrust

(*let _ = Flags.resource_errors_as_warnings := true*)
(* let _ = Flags.display_includes := true *)

let _ = Run.script_cpp (fun () ->
  Resources.show_ast ();
  Resources.show ();
)