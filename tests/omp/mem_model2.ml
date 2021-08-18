open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [Num_threads "2"] [tAfter; cVarDef "flag"];
  !! Omp.flush ["flag";"data"] [tAfter; sInstr "data ="];
  !! Omp.flush ["flag"] [tAfter; sInstr "flag = 1"];
  !! Omp.flush ["flag";"data"] [tBefore; cWhile ()];
  !! Omp.flush ["flag";"data"] [tFirst; cWhile ();dBody];
  !! Omp.flush ["flag";"data"] [tAfter; cWhile ()];
  !! Omp.barrier [tBefore; tIndex ~nb:2 1;cIf ()];
)
