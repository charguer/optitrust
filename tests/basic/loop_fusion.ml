open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->

  !! Loop.fusion_on_block [cLabel "tofusion"];
  !! Trace.alternative (fun () ->
     !! Loop.fusion_on_block [cLabel "tofusion"];
     !!());


  (* TODO: move to combi:
    !! Loop.fusion [tIndex ~nb:2 0; cFor "i"]);

    !! Trace.alternative (fun () ->
      !! Sequence.sub 3 ~label:"tofusion" [tIndex ~nb:3 0; cFor "i"];
      !! Loop.fusion_on_block [cLabel "tofusion"];)
  *)

)
