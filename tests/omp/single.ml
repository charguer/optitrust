open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.parallel [] [tBefore; occIndex ~nb:2 0; cFunDef "single_example"; dBody; cSeq ()];
  !! Omp.single [] [tFirst;occIndex ~nb:2 0; cFunDef "single_example"; dBody; cSeq ()];
  !! Omp.single [] [tAfter; cFun "work1"];
  !! Omp.single [Nowait] [tAfter; cFun "work2"];
)
