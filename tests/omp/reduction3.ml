open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [Shared ["a"]; Private ["i"]] [tAfter; cVarDef "a"];
  !! Omp.master [tBefore; sInstr "a = 0"];
  !! Omp.for_ [Reduction (Plus, ["a"])] [tBefore; cFor "i"];
  !! Omp.single [] [tBefore; cFun "printf"];

)
