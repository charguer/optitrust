open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.task [Shared ["x"]; Depend (Out ["x"])] [tBefore; sInstr "x = 1"];
  !! Omp.task [Shared ["x"]; Depend (Out ["x"])] [tBefore; sInstr "x = 2"];
  !! Omp.taskwait [tAfter; sInstr "x = 2"];
  !! Omp.single []  [tAfter; cVarDef "x"];
  !! Omp.parallel []  [tAfter; cVarDef "x"];
)