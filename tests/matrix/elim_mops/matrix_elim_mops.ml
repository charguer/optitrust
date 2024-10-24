open Optitrust
open Prelude

let _ = Flags.check_validity := true
(* TODO:
let _ = Flags.recompute_resources_between_steps := true *)

let _ = Run.script_cpp (fun () ->
  (* TODO: STACK ARRAY + MMEMCPY *)
  !! Matrix.elim_mops [];
  (* cFunDefs ["outer_alloc"; "inner_alloc"; "copy"] *)
)
