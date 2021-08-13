open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target [Map_c (To, ["B[0:N]";"C[0:N]"]); Map_c (ToFrom, ["sum"])] [tAfter; cVarDef "sum"];
  !! Omp.teams [Num_teams "num_teams";Thread_limit "block_threads";Reduction (Plus,["sum"])] [tBefore; cFor_c "i0"];
  !! Omp.distribute [] [tBefore; cFor_c "i0"];
  !! Omp.parallel_for [Reduction(Plus, ["sum"])] [tBefore;cFor_c "i"];
  
)
 