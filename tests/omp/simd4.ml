open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.simd [Safelen 16] [tBefore; cFor "i"];
)