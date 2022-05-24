open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.declare_target [] [tBefore; cVarDef "Q"];
  !! Omp.declare_simd [Uniform ["i"]; Linear (["k"],0); NotInbranch ] [tBefore; cFunDef "P"];
  !! Omp.end_declare_target [tAfter; cFunDef "P"];
  !! Omp.target [Map_c (ToFrom, ["tmp"])] [tBefore; cFor "i"];
  !! Omp.parallel_for [Reduction (Plus, ["tmp"])] [tBefore;cFor "i"];
  !! Omp.parallel_for [Reduction (Plus, ["tmp1"])] [tBefore;cFor "k"];
)
