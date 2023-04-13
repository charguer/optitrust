open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.declare_target [tBefore; cFun "vec_mult"];
  !! Omp.end_declare_target [tAfter; cVarDef "N"];
  !! Omp.target ~clause:[Device "42"; Map_c (No_map,["p[:N]";"v1[:N]";"v2[:N]]"])] [tAfter; cFun "init_vars"];
  !! Omp.parallel_for ~clause:[Private ["i"]; Num_threads "nthreads"] [tBefore; cFor "i"];
)
