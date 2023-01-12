open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

     let tg_loop = [cFor_c ""] in 
  !! Omp.distribute_parallel_for_simd tg_loop;
  !! Omp.target_teams ~clause:[Map_c (To, ["v1[0:N]";"v2[:N]"]); Map_c (From, ["p[0:N]"])] tg_loop;

)
