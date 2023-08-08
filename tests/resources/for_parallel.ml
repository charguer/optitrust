open Optitrust
open Target

(*let _ = Flags.resource_errors_as_warnings := true*)

(* FIXME: avoid inlining *)
let _ = Run.script_cpp ~inline:["../../include/optitrust.h"] (fun () ->
    show_ast ();
    show_res ();
)
