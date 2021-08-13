open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target_data [Map_c (No_map, ["Q[0:rows][0:cols]"])] [tBefore; cFor_c "k"];
  !! Omp.target [Map_c (ToFrom, ["tmp"])] [tBefore;cFor_c "i"];
  !! Omp.parallel_for [Reduction (Plus, ["tmp"])] [tBefore; cFor_c "i"];
  !! Omp.target [] [tBefore;cFor_c "j"];
  !! Omp.parallel_for [] [tBefore; cFor_c "j"];
)
