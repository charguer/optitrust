open Optitrust
open Target
open Resources

(*let _ = Flags.resource_errors_as_warnings := true*)
(*let _ = Flags.always_name_resource_hyp := true
let _ = Flags.display_resources := true*)

let _ = Run.script_cpp (fun () ->
    show_ast ();
    show_res ();

    recompute_all_resources ();
    show_computed_res ();
    loop_minimize [cFor "i"];
)
