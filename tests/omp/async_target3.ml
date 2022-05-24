open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [] [tBefore; cFun "init"];
  !! Omp.master [tBefore; cFor "i"];
  !! Omp.target_teams_distribute_parallel_for [Nowait] [tBefore; cFor "i"];
  !! Omp.for_ [Schedule (Dynamic, "chunk")] [tBefore; cFor "j"];
)
