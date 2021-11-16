open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.declare_simd [Inbranch] [tBefore; cFunDef "fib"];
  !! Omp.simd [] [tBefore; occIndex ~nb:2 0;cFor "i"];
  !! Omp.simd [] [tBefore; occIndex ~nb:2 1;cFor "i"];
)
