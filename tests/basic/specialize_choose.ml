open Optitrust
open Target

(* LATER: wait until we have support for templates to properly implementation CHOOSE
   and document this function. *)

let _ = Run.script_cpp (fun _ ->

  !! Specialize_basic.choose "xa" [cChoose];
)