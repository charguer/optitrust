open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.declare_target ~clause:[Link ["sp";"sv1";"sv2"];Link ["dp";"dv1";"dv2"]] [cVarDef "sp"];
  
  !! Omp.declare_target [cTopFunDef "s_vec_mult_accum"];
  !! Omp.parallel_for [nbMulti; cFor_c ""];
  !! Omp.end_declare_target [cTopFunDef "main"];
  
  !! Omp.target ~clause:[Map_c (To, ["sv1";"sv2"]); Map_c (From, ["sp"])] [cFun "s_vec_mult_accum"];
  !! Omp.target ~clause:[Map_c (To, ["dv1";"dv2"]); Map_c (From, ["dp"])] [cFun "d_vec_mult_accum"];
)
