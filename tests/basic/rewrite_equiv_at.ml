open Optitrust
open Target
let _ = Run.script_cpp (fun _ ->

  !! Rewrite.equiv_at "double a; double b; int k; # a + k * b == b * k  + a" [cSetVar "res"; dRHS];
  !! Rewrite.equiv_at "double a; int k; # a + k * a == (k + 1) * a" [cSetVar "res1"; dRHS];
  !! Rewrite.equiv_at "double a; int k; # a + k * a == (k + 1) * a" [cVarDef "res2"; dBody];
)
