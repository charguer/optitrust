open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
    !! Loop.fusion ~nb:3 [tIndex ~nb:3 0; cFor "i"];
    !! Trace.alternative (fun () ->
      !! Sequence_basic.intro 3 ~label:"tofusion" [tIndex ~nb:3 0; cFor "i"];
      !! Loop_basic.fusion_on_block [cLabel "tofusion"];
      !!());
)
