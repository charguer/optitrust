open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [cFun "vec_mult"];
  !! Omp.target_data [Map_c (To, ["v1[0:N]";"v2[:N]"])] [tAfter; cFun "init"];
  !! Omp.target [Map_c (To, ["v3[0:N]";"v4[:N]"])] [tBefore;cFor "i"];
  !! Omp.parallel_for [] [tBefore; cFor "i"];
)
