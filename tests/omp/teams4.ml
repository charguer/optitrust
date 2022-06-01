open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target_teams [Map_c (To, ["B[0:N]";"C[0:N]"]); Map_c (ToFrom, ["sum"])] [tAfter; cVarDef "sum"];
  !! Omp.teams [Num_teams "8";Thread_limit "16";Reduction (Plus,["sum"])] [tBefore; cFor "i"];
  !! Omp.distribute_parallel_for [Reduction(Plus, ["sum"]); Dist_schedule (Static, "1024"); Schedule (Static, "64")] [tBefore; cFor "i"];
)
