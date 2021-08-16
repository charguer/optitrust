open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.declare_target [] [tBefore; cVarDef "p"];
  !! Omp.end_declare_target [] [tAfter; cVarDef "p"];
  !! Omp.target_update [To_c ["v1";"v2"]] [tAfter; cFun "init"];
  !! Omp.target [] [tBefore;cFor "i"];
  !! Omp.parallel_for [] [tBefore;cFor "i"];
  !! Omp.target_update [From_c ["p"]] [tAfter; cFor "i"];
)
