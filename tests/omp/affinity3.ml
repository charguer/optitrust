open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [cFunDef "main"; cFun "work"];
  !! Omp_basic.parallel ~clause:[Proc_bind Close; Num_threads "4"] [cSeq ~args:[[cFun "work"]] ()];

)
