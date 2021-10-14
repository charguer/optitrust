open Optitrust
open Target
let _ = Run.script_cpp (fun _ -> 

  !! Rewrite.equiv_at "double a, b; int k; a + k * b == b * k  + a" [cSetVar "res1"; dRHS];
)


