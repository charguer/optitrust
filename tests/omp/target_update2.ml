open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 2 [cFor  "i"];
  !! Omp.target_data [Map_c (To, ["v1[:N]";"v2[:N]"]);Map_c (From, ["p0[0:N]"])] [tAfter; cFun "init"];
  !! Omp.target [] [tBefore;cFor "i"];
  !! Omp.parallel_for [] [tBefore; cFor "i"];
  !! Omp.target_update [If "changed"; To_c ["v1[:N]"]] [tAfter; occIndex ~nb:2 0; sInstr "changed ="];
  !! Omp.target_update [If "changed"; To_c ["v2[:N]"]] [tAfter; occIndex ~nb:2 1; sInstr "changed ="];
  !! Omp.target [] [tBefore;cFor "j"];
  !! Omp.parallel_for [] [tBefore; cFor "j"];
)
