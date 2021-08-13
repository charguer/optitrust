open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Omp.atomic (Some Capture) [tAfter; cVarDef "old"];
  !! Omp.flush [] [tBefore; cFun "work"];  
  !! Omp.flush [] [tAfter; cFun "work"];  
)
