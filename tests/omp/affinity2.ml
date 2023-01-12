open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [cFunDef "foo"; cFun "work"];
  !! Omp_basic.parallel ~clause:[Num_threads "16"; Proc_bind Spread] [cSeq ~args:[[cFun "work"]] ()];

)
