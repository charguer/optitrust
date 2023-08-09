open Optitrust
open Target

(* FIXME: avoid inlining *)
let _ = Run.script_cpp ~inline:["../../include/optitrust.h"] (fun () ->
    show_ast ();
    Resources.show ();
)
