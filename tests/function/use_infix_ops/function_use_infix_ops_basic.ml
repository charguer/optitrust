open Optitrust
open Prelude

(* ARTHUR: add an efficient mechanism for targeting all potential infix ops in depth *)

let _ = Run.script_cpp (fun _ ->

  !! Function_basic.use_infix_ops_at [nbMulti; cWriteVar "x"];

)
