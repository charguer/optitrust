open Optitrust
open Target
open Resources

(*let _ = Flags.resource_errors_as_warnings := true*)
let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := false
let _ = Flags.display_resources := true
let _ = Flags.always_name_resource_hyp := true

let _ = Run.script_cpp (fun () ->
    !! loop_minimize [cFunBody "default_contract"; cFor "i"];
    !! loop_minimize [cFunBody "iter_contract"; cFor "i"];
    !! loop_minimize [cFunBody "produced_uninit_used_ro"; cFor "i"]; (* FIXME *)
    !! loop_minimize ~shallow:true [cFunBody "nested_loops"; cFor "i"]; (* Should do nothing *)
    Resources.ensure_computed ();
    !! loop_minimize [cFunBody "nested_loops"; cFor "i"];
)
