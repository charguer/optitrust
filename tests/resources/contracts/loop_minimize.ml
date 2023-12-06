open Optitrust
open Target
open Resources

(*let _ = Flags.resource_errors_as_warnings := true*)
let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun () ->
    !! loop_minimize [cFunBody "h"; cFor "i"];
    !! loop_minimize [cFunBody "f"; cFor "i"];
    !! loop_minimize [cFunBody "g"; cFor "i"]; (* FIXME *)
)
