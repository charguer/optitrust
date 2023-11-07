open Optitrust
open Target
open Resources

(*let _ = Flags.resource_errors_as_warnings := true*)
(*let _ = Flags.always_name_resource_hyp := true
let _ = Flags.display_resources := true*)

let _ = Run.script_cpp (fun () ->
    show_ast ();
    Resources.show ();

    !! loop_minimize [cFor "i"];
    Resources.show ();
)
