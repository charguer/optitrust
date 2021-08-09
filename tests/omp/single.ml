open Optitrust
open Target

(* TODO: Fix the issue with sequences *)
(* TODO: Fix the issue with matching internal function calls *)
let _ = Run.script_cpp (fun _ ->
  !! Omp.parallel [] [tBefore; tIndex ~nb:2 0; cFunDef "single_example"; dBody; cSeq ()];
  !! Omp.single [] [tFirst;tIndex ~nb:2 0; cFunDef "single_example"; dBody; cSeq ()];
  !! Omp.single [] [tAfter; cFun "work1"];
  !! Omp.single [Nowait] [tAfter; cFun "work2"];
)
