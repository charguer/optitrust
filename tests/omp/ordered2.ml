open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.ordered  [] [tBefore; cFor "i"];
  !! Omp.ordered  [] [tBefore; occIndex ~nb:2 0; cFun "work"];
  !! Omp.ordered  [] [tBefore; occIndex ~nb:2 1; cFun "work"];
  
)
