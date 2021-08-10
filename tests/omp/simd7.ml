open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.declare_simd [Inbranch] [tBefore; cFunDef "fib"];
  !! Omp.simd [] [tBefore; tIndex ~nb:2 0;cFor_c "i"];
  !! Omp.simd [] [tBefore; tIndex ~nb:2 1;cFor_c "i"];
)
