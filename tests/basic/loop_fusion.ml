open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  (* fuse three loops *)
  !! Loop.fusion ~nb:3 [cFunDef "main"; cFor "i" ~body:[sInstr "t[i]"]];

  !! Trace.alternative (fun () ->
    (* default is two loops *)
    !! Loop.fusion [cFunDef "main"; cFor "i" ~body:[sInstr "t[i]"]];
    !!());

  (* LATER: try to improve error: "cannot fuse 3 loops as requested, only 2 loops are found"
    !! Loop.fusion ~nb:3 [cFunDef "main"; tIndex ~nb:3 1; cFor "i"]; *)

  (* fuse loops inside a labelled block *)
  !! Sequence_basic.intro ~label:"tofusion" 3 [cFunDef "fusion_on_block"; cFor "i" ~body:[sInstr "t[i]"]];
  !! Loop_basic.fusion_on_block [cLabel "tofusion"];
  )
