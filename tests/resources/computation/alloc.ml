open Optitrust

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
(*let _ = Flags.resource_errors_as_warnings := true*)
let _ = Flags.strip_trace := false

let _ = Run.script_cpp (fun () -> ())
