Fopen Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.fold  "i" "0" "4" "1" [cLabel "iterations"; dBody];

  !! Sequence_basic.intro ~mark:"instrs" 3 [cCellWrite ~base:[cVar "values"] [cInt 0]];
  !! Loop_basic.fold  "k" "0" "3" "1" [cMark "instrs"];
)
