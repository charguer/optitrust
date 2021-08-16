open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.declare_target [] [tBefore; cVarDef "Q"];
  !! Omp.end_declare_target [] [tAfter; cFunDef "Pfun"];
  !! Omp.target_update [To_c ["Q"]] [tBefore; cFor "i"];
  !! Omp.target [Map_c (ToFrom, ["tmp"])] [tBefore; cFor "i"];
  !! Omp.parallel_for [Reduction (Plus, ["tmp"])] [tBefore;cFor "i"];
)
