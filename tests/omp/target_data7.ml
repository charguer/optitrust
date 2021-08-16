open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [cFor  "i"];
  !! Omp.target_data [Map_c (From, ["p0[0:N]"])] [tAfter; cFun "init"];
  !! Omp.target [If "N>THRESHOLD";Map_c (To ["v1[:N]";"v2[:N]"])] [tBefore;cFor "i"];
  !! Omp.parallel_for [] [tBefore; cFor "i"];
)
