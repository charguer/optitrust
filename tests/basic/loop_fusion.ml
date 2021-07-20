open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  (* show [nbMulti; cSeq ~args:[cFor "i"]()]; *)

  (* TODO: tIndex for selecting one among multiple
  !! Sequence.sub 2 ~label:"tofusion" [tIndex ~nb:2 0; cFor "i"];
  !! Loop.fusion_on_block [cLabel "tofusion"]

  TODO: fusion any number of loops that are in the "tofusion" block.
  *)
  !! Loop.fusion [cSeq ~args_pred:(target_list_one_st (cFor "i")) ()];
)

(* TODO: high-level function
    - takes the path p to one loop
    - takes a number nb of loops, by default nb=2
    - expects the path p to point to a for loop, followed by 2 (or nb) for-loops
    - it calls Sequence.sub nb on p, with label "__optitrust_342"
    - fusion_on_block on cLabel "__optitrust_342"

   LATER: introduce fusion which checks that the ranges are the same *)