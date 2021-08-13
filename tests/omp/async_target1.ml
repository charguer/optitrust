open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.declare_target [] [tBefore; cFunDef "F"];
  !! Omp.end_declare_target [tAfter; cFunDef "F"];
  !! Omp.task [Shared ["z"]] [tBefore; cFor_c "i"];
  !! Omp.target_teams [Map_c (From, ["Z[C:CHUNKSZ]"])] [tBefore; cFor_c "i"];
  !! Omp.parallel_for [] [tBefore; cFor_c "i"];
  !! Omp.taskwait [tAfter; cFor_c "i"];
)
 