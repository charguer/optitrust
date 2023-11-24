open Optitrust
open Target
open Resources

(*let _ = Flags.resource_errors_as_warnings := true*)
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun () ->
    Resources.show ();
    !! loop_parallelize_reads [cFor "i"];
    !! loop_minimize [cFor "j"];
    !! loop_parallelize_reads [cFor "j"];
)
