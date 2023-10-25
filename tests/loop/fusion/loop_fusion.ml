open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  (* fuse a given number of loops *)
  !! Loop.fusion ~nb:3 [cFunDef "fusion_on_block"; occIndex ~nb:3 0; cFor "i"];

  (* fuse two loops when targeting the first one *)
  !! Loop.fusion ~nest_of:2 [cFunDef "main"; cFor "i" ~body:[sInstr "t[i]"]];
)
