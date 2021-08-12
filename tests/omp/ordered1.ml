open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.ordered [tBefore; cFun "printf"];
  !! Omp.parallel_for [Ordered_c; Schedule (Dynamic,0)] [tBefore; cFor_c "i"];
)
