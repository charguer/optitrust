open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->

  (* fuse a given number of loops *)
  !! Loop.fusion ~nb:3 [cFunDef "fusion_on_block"; occIndex ~nb:3 0; cFor "i"];

  (* fuse two loops when targeting the first one *)
  !! Loop.fusion [cFunDef "main"; cFor "i" ~body:[sInstr "t[i]"]];

  (* implementation details *)
  !! Trace.alternative (fun () ->
    !! Sequence_basic.intro ~mark:"tofuse" 3 [cFunDef "fusion_on_block"; cFor "i" ~body:[sInstr "t[i]"]];
    !! Loop_basic.fusion_on_block [cMark "tofuse"];
    !!());
)
