open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.declare_target [Link ["sp";"sv1";"sv2"];Link ["dp";"dv1";"dv2"]] [tBefore; cVarDef "sp"];
  !! Omp.declare_target [] [tBefore;cFunDef "s_vec_mult_accum"];
  !! Omp.parallel_for [] [tBefore; cFunDef "s_vec_mult_accum";cFor "i"];
  !! Omp.parallel_for [] [tBefore; cFunDef "d_vec_mult_accum";cFor "i"];
  !! Omp.end_declare_target [tBefore;cFunDef "main"];
  !! Omp.target [Map_c (To, ["sv1";"sv2"]); Map_c (From, ["sp"])] [tAfter; cFun "s_init"];
  !! Omp.target [Map_c (To, ["dv1";"dv2"]); Map_c (From, ["dp"])] [tAfter; cFun "d_init"];
)
 