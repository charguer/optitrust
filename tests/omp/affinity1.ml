open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Sequence_basic.intro 1 [cFun "work"];
  !! Omp.parallel ~clause:[Proc_bind Spread; Num_threads "4"] [cSeq ~args:[[cFun "work"]] ()];
  
)
