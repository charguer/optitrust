open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel_for [Private ["i"]] [tBefore; cFor "i"];
)
