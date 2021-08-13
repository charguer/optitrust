open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [Num_threads "10"] [tFirst; cFunDef "main"; dBody];
  !! Omp.set_dynamic 1 [tFirst; cFunDef "main"; dBody];
)