open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic. intro 3 [cFor_c "i"];
  !! Omp.target_data [Map_c (From, ["p[0:N]"])] [tAfter; cFun "init"];
  !! Omp.target [Map_c (To, ["v1[:N]";"v2[N]"])] [tBefore;cFor_c "i"];
  !! Omp.parallel_for [] [tBefore; cFor_c "i"];
  !! Omp.target [Map_c (To, ["v1[:N]";"v2[N]"])] [tBefore;cFor_c "j"];
  !! Omp.parallel_for [] [tBefore; cFor_c "j"];
)
