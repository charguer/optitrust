open Optitrust
open Prelude

let _ = Flags.check_validity := true
(* TODO:
  let _ = Flags.recompute_resources_between_steps := true *)

let _ = Run.script_cpp (fun () ->
  !! Matrix.elim_mops [cFunDef "outer_alloc"];
  !! Matrix.elim_mops [cFunDef "inner_alloc"];
)
