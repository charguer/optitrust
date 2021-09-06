open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 2 [cFor  "i"];
  !! Omp.target_data [Map_c (To, ["v1[:N]";"v2[:N]"]);Map_c (From, ["p0[0:N]"])] [tAfter; cFun "init"];
  !! Omp.target [] [tBefore;cFor "i"];
  !! Omp.parallel_for [] [tBefore; cFor "i"];
  !! Omp.target_update [To_c ["v1[:N]";"v2[:N]"]] [tAfter; cFun "init_again"];
  !! Omp.target [] [tBefore;cFor "j"];
  !! Omp.parallel_for [] [tBefore; cFor "j"];
)