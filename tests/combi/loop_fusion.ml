open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  
  !! Loop.fusion ~nb:3 [cFunDef "fusion_on_block"; occIndex ~nb:3 0; cFor "i"];
  !! Loop.fusion [cFunDef "main"; cFor "i" ~body:[sInstr "t[i]"]];
  !! Trace.alternative (fun () ->
    (* default is two loops *)
    !! Sequence_basic.intro ~mark:"tofusion" 3 [cFunDef "fusion_on_block"; cFor "i" ~body:[sInstr "t[i]"]];
    !! Loop_basic.fusion_on_block [cMark "tofusion"];  
    !!());
)
