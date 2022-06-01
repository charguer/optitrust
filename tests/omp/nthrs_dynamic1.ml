open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel ~clause:[Num_threads "10"] [cSeq ~args:[[cFun "printf"]] ()];
  !! Omp.set_dynamic 0 [tFirst; cFunDef "main"; dBody];

)
