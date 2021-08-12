open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [Private ["a"]] [tAfter; cVarDef "a"];
  !! Omp.parallel_for [Private ["a"]] [tBefore; cFor_c "i"];
)
