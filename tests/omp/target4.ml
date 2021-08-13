open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target [Map_c (To, ["v1[0:N]";"v2[:N]"]);Map_c (From, ["p[0:N]"])] [tBefore; cFor_c "i"];
  !! Omp.parallel_for [] [tBefore; cFor_c "i"];
)
