open Optitrust
open Prelude
open Target

(* LATER: wait until we have support for templates to properly implementation CHOOSE
   and document this function. *)

let _ = Run.script_cpp (fun () ->
  let a = find_var_in_current_ast ~target:[cChoose] "a" in
  !! Specialize_basic.choose a [cChoose];

)
