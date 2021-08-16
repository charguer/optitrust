open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target [Map_c (To ["v1";"v2"]);Map_c (From, ["p"])] [tBefore; cFor "i"];
  !! Omp.parallel_for [] [tBefore; cFor "i"];
)
