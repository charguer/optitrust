open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true

let _ = Run.script_cpp (fun () ->
  !! Cleanup.std ();
  !! Arith.(simpl_rec expand) [];
  !! Arith.(simpl_rec (compose [euclidian; gather_rec; compute])) [];
)
