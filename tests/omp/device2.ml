open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target [If "do_offload"; Map_c (To, ["v1[0:N]";"v2[:N]"]); Map_c (From, ["p[0:N]"])] [tBefore; cFor "i"];
  !! Omp.parallel_for [If "N > 10000"; Private ["i"]] [tBefore; cFor "i"];
)
