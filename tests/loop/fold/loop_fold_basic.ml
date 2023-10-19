open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.fold  ~index:"i" ~start:0 ~step:1 [cLabel "iterations"];
  !! Sequence_basic.intro ~mark:"instrs" 3 [cCellWrite ~base:[cVar "values"] ~index:[cInt 0] ()];
  !! Loop_basic.fold  ~index:"k" ~start:0 ~step:1 [cMark "instrs"];

)
