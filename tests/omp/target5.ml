open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target [If "N > THRESHOLD1";Map_c (To, ["v1[0:N]";"v2[:N]"]);Map_c (From, ["p[0:N]"])] [tBefore; cFor "i"];
  !! Omp.parallel_for [If "N > THRESHOLD2"] [tBefore; cFor "i"];
)
