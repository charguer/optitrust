open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true

let _ = Run.script_cpp (fun () ->
  !! Resources.ensure_computed ();
  !! Cleanup.std ();
  !! Arith.(simpl_rec expand_rec) [];
  !! Arith.(simpl_rec (compose [euclidian; compute])) [];
  !! Arith.(simpl_rec gather_rec) [];
  !! Arith.(simpl_rec compute) [];
  !! Arith.(simpl2 ~indepth:true sort) [];  (* LATER: simpl2_rec *)
)
