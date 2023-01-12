open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target_data ~clause:[Map_c (No_map, ["Q[0:rows][0:cols]"])] [cFor "k"];
     let tg_loopi1 = [occIndex 0; cFor "i"] in 
  !! Omp.parallel_for ~clause:[Reduction (Plus, ["tmp"])] tg_loopi1;
  !! Omp.target ~clause:[Map_c (ToFrom, ["tmp"])] tg_loopi1;
  
     let tg_loopi2 = [occIndex 1; cFor "i"] in 
  !! Omp.parallel_for tg_loopi2;
  !! Omp.target tg_loopi2;

)
