open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    let tg_loop = [cFor_c ""] in 
  !! Omp.distribute_parallel_for ~clause:[Reduction(Plus, ["sum"])] tg_loop;
  !! Omp.target_teams ~clause:[Map_c (To, ["B[0:N]";"C[0:N]"]); Defaultmap (ToFrom, ["scalar"])] tg_loop;
)
