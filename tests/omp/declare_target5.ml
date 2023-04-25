open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  
  !! Omp.declare_target [cVarDef "Q"];
  !! Omp.declare_simd ~clause:[Uniform ["i"]; Linear (["k"],0); NotInbranch ] [cTopFunDef "P"];
  !! Omp.end_declare_target [cTopFunDef "accum"];

    let tg_loopi = [occIndex 1; cFor_c ""] in

  !! Omp.parallel_for ~clause:[Reduction (Plus, ["tmp"])] tg_loopi;
  !! Omp.target ~clause:[Map_c (ToFrom, ["tmp"])] tg_loopi;

    let tg_loopk = [occIndex 0; cFor_c ""] in
  !! Omp.parallel_for ~clause:[Reduction (Plus, ["tmp1"])] tg_loopk;

)
