open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.teams [Num_teams "2"] [tAfter; cVarDef "sum1"];
  !! Omp.target [Map_c (To, ["B[:N]";"C[:N]"]); Map_c (ToFrom, ["sum0";"sum1"])] [tAfter; cVarDef "sum1"];
  !! Omp.parallel_for [Reduction(Plus, ["sum0"])] [tBefore; occIndex ~nb:2 0;cFor "i"];
  !! Omp.parallel_for [Reduction(Plus, ["sum1"])] [tBefore; occIndex ~nb:2 1;cFor "i"];
)
 