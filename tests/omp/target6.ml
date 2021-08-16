open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel_for [If "target: N > THRESHOLD1"; If "target: N > THRESHOLD2";Map_c (To ["v1[0:N]";"v2[:N]"]);Map_c (From, ["p[0:N]"])] [tBefore; cFor "i"];
)
