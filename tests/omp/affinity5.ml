open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [cFunDef "main"; cCall "work"];
  !! Omp_basic.parallel ~clause:[Proc_bind Master_pb; Num_threads "4"] [cSeq ~args:[[cCall "work"]] ()];

)
