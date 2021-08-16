open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.declare_simd [Uniform ["fact"]] [tBefore; cFunDef "add1"];
  !! Omp.declare_simd [Uniform ["a";"b";"fact"]; Linear (["i"],1)] [tBefore; cFunDef "add2"];
  !! Omp.declare_simd [Uniform ["a";"b";"fact"]; Linear (["i";"b"],1)] [tBefore; cFunDef "add3"];
  !! Omp.simd [Private ["tmp"]] [tBefore; cFunDef "work"; cFor "i"];
)
