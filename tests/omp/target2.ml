open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target [Map_c (No_map, ["v1";"v2";"p"])] [tBefore; cFor_c "i"];
  !! Omp.parallel_for [] [tBefore; cFor_c "i"];
)
