open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target [Map_c (To, ["v1";"v2"]);Map_c (From, ["p"])] [tBefore; cFor_c "i"];
  !! Omp.parallel_for [] [tBefore; cFor_c "i"];
)
