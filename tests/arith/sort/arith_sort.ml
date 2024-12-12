open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun _ ->

!!Cleanup.std ();

  !! Arith_basic.(simpl2 sort) [nbMulti; cWriteVar "x"; dRHS];
  !! Arith_basic.(simpl2 ~indepth:true sort) [nbMulti; cWriteVar "y"; dRHS];

)
