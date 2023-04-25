open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    let tg_loopi0 = [occIndex 1; cFor_c ""] in 
  !! Omp.distribute tg_loopi0;
  !! Omp.teams ~clause:[Num_teams "num_teams";Thread_limit "block_threads";Reduction (Plus,["sum"])] tg_loopi0;
  !! Omp.target ~clause:[Map_c (To, ["B[0:N]";"C[0:N]"]); Map_c (ToFrom, ["sum"])] tg_loopi0;
    let tg_loopi1 = [occIndex 0; cFor_c ""] in 
  !! Omp.parallel_for ~clause:[Reduction(Plus, ["sum"])] tg_loopi1;

)
