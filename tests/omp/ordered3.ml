open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.for_ [Ordered_c 0] [tBefore; cFor_c "i"];
  !! Omp.ordered  [] [tBefore; tIndex ~nb:2 0; cFun "work"];
  !! Omp.ordered  [] [tBefore; tIndex ~nb:2 1; cFun "work"];
)
