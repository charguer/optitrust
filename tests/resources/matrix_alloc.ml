open Optitrust
open Target

(* FIXME: avoid inlining *)
let _ = Run.script_cpp (fun () ->
    show_ast ();
    Resources.show ();
)
