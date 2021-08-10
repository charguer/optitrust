open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.simd [Private ["tmp"]; Reduction(Plus, ["sum"]) ] [tBefore; cFor_c "i"];
)