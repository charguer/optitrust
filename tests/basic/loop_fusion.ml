open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Loop.fusion [tIndex ~nb:2 0; cFor "i"];
  (* !! Sequence.sub 2 ~label:"tofusion" [tIndex ~nb:2 0; cFor "i"]; *)
  (* !! Loop.fusion_on_block [cLabel "tofusion"]; *)
  
)

(* TODO: high-level function
    - takes the path p to one loop
    - takes a number nb of loops, by default nb=2
    - expects the path p to point to a for loop, followed by 2 (or nb) for-loops
    - it calls Sequence.sub nb on p, with label "__optitrust_342"
    - fusion_on_block on cLabel "__optitrust_342"

   LATER: introduce fusion which checks that the ranges are the same *)