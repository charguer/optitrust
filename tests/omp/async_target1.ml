open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.declare_target [] [tBefore; cFunDef "F"];
  !! Omp.end_declare_target [tAfter; cFunDef "F"];
  !! Omp.task [Shared ["z"]] [tBefore; cFor "i"];
  !! Omp.target_teams [Map_c (From, ["Z[C:CHUNKSZ]"])] [tBefore; cFor "i"];
  !! Omp.parallel_for [] [tBefore; cFor "i"];
  !! Omp.taskwait [tAfter; cFor "i"];
)
 