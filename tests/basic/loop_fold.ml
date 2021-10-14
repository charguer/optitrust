open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 


  !! Sequence_basic.intro ~mark:"instrs" 3 [cIndexSet ~base:[cVar "values"] [cInt 0]];
  !! Loop_basic.fold  "k" "0" "3" "1" [cMark "instrs"];
)
