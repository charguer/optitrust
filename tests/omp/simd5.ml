open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.simd [Collapse 16; Private ["tmp"]] [tBefore; cFor "i"];
)
