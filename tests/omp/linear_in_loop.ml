open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 
  
  !! Omp.parallel [] [tIndex ~nb:2 1; tBefore; cFor "i"];
  !! Omp.for_ [Linear (["j"],1)] [tIndex ~nb:2 1; tBefore; cFor "i"];
)