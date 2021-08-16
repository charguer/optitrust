open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.simd [] [tBefore;cFor "i"] ;
)
