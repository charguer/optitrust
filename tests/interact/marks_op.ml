open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Marks.add "M1" [nbMulti; cFor "i"];
  !! Marks.add "M2" [nbMulti; cFor "i"];
  !! Marks.add "M3" [nbMulti; cFunDef "main"; cFor "i"];
  !! Marks.remove "M1" [nbMulti; cFor "i"];
  !! Marks.add "M4" [nbMulti; cFor "i"];
  !! Marks.add "M5" [sInstr "i++"; tAfter];
  !! Marks.remove "M5" [cMark "M5"];
  !! Marks.clean [nbMulti; cFor "i"];
)
