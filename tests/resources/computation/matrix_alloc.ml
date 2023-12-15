open Optitrust
open Target

(* FIXME: avoid inlining *)
let _ = Run.script_cpp (fun () ->
    Resources.show_ast ();
    Resources.show ();
)
