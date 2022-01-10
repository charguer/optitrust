open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [Num_threads "3"] [tAfter; cVarDef "flag"];
  !! Omp.atomic (Some Update) [tBefore; occIndex ~nb:2 0; sInstr "flag++"];
  !! Omp.flush ["flag"] [tBefore; occIndex ~nb:2 0; cWhile ()];
  !! Omp.flush ["flag"] [tFirst; occIndex ~nb:2 0;cWhile ();dBody];
  !! Omp.atomic (Some Update) [tBefore; occIndex ~nb:2 1; sInstr "flag++"];
  !! Omp.flush ["flag"] [tBefore; occIndex ~nb:2 1; cWhile ()];
  !! Omp.flush ["flag"] [tFirst; occIndex ~nb:2 1;cWhile ();dBody];
)
