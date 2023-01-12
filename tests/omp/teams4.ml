open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

     let tg_loop = [cFor_c ""] in 
  !! Omp.distribute_parallel_for ~clause:[Reduction(Plus, ["sum"]); Dist_schedule (Static, "1024"); Schedule (Static, "64")] tg_loop;
  !! Omp.teams ~clause:[Num_teams "8";Thread_limit "16";Reduction (Plus,["sum"])] tg_loop;
  !! Omp.target_teams ~clause:[Map_c (To, ["B[0:N]";"C[0:N]"]); Map_c (ToFrom, ["sum"])] tg_loop;

)
