open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel ~clause:[Num_threads "10"] [cSeq ~args:[[cCall "printf"]] ()];
  !! Omp.set_dynamic 1 [tFirst; cFunBody "main"];

)
