open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [cFunDef "foo"; cCall "work"];
  !! Omp_basic.parallel ~clause:[Num_threads "16"; Proc_bind Close] [cSeq ~args:[[cCall "work"]] ()];

)
