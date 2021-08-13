open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target_teams [Map_c (To, ["B[0:N]";"C[0:N]"]); Defaultmap (ToFrom, ["scalar"])] [tAfter; cVarDef "sum"];
  !! Omp.distribute_parallel_for [Reduction(Plus, ["sum"])] [tBefore; cFor_c "i"];
)
 