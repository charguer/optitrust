open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.task [Mergeable] [tAfter; cVarDef "x"];
  !! Omp.taskwait [tBefore; cFun "printf"];
)
