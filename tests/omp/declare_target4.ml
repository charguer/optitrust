open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.declare_target [cVarDef "Q"];
  !! Omp.end_declare_target [cTopFunDef "accum"];
     let tg_loop = [cFor "i"] in 
  !! Omp.parallel_for ~clause:[Reduction (Plus, ["tmp"])] tg_loop;
  !! Omp.target ~clause:[Map_c (ToFrom, ["tmp"])] tg_loop;
  !! Omp.target_update ~clause:[To_c ["Q"]] tg_loop;
  
)
