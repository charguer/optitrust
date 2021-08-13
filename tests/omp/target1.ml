open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target [] [tAfter; cFun "init"];
  !! Omp.parallel_for [Private ["i"]] [tBefore; cFor_c "i"];
  
)