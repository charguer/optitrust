open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.declare_target [] [tBefore; cFunDef "fib"];
  !! Omp.end_declare_target [tAfter; cFunDef "fib"];
  !! Sequence_basic.intro 1 [cFun "fib"];
  !! Omp.target [If "n > THRESHOLD"] [tFirst; cFunDef "fib_wrapper"; dBody];
)
