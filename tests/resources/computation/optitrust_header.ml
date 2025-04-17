open Optitrust

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
(*let _ = Flags.debug_parsing_serialization := true*)
(*let _ = Flags.resource_errors_as_warnings := true*)

let _ = Run.script_cpp ~inline:["../../../include/optitrust.h"] (fun () -> ())
