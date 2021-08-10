open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.declare_simd [Linear (["p"], 1); NotInbranch] [tBefore; cFunDef "foo"];
  !! Omp.simd [] [tBefore; cFunDef "myaddint"; cFor_c "i"];
  !! Omp.declare_simd [Linear (["p"], 1); Inbranch] [tBefore; cFunDef "goo"];
  !! Omp.simd [] [tBefore; cFunDef "myaddfloat"; cFor_c "i"];
  
)
