open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.task [] [tBefore; cFun "task_body"];
  !! Omp.parallel [Private ["y"]] [tFirst; cFunDef "test"; dBody];
)