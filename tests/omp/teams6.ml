open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target_teams [Map_c (To, ["v1[0:N]";"v2[:N]"]); Map_c (From, ["p[0:N]"])] [tAfter; cFun "init"];
  !! Omp.distribute_parallel_for_simd [] [tBefore; cFor "i"];
)
