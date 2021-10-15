open Optitrust
open Target
let _ = Run.script_cpp (fun _ ->

  !! Rewrite.equiv_at "const double a, b; int k; a + k * b == b * k  + a" [cSetVar "res"; dRHS];
  !! Rewrite.equiv_at "const double a; int k; a + k * a == (k + 1) * a" [cSetVar "res1"; dRHS];
  !! Rewrite.equiv_at "const double a; int k; a + k * a == (k + 1) * a" [cVarDef "res2"; dBody];


  (* TODO: remove those *)
  !! Rewrite.equiv_at "double a, b; int k; a + k * b == b * k  + a" [cSetVar "res"; dRHS];
  !! Rewrite.equiv_at "double a, b; int k; a + k * b == b * k  + a" [cSetVar "res1"; dRHS];
)


(*
  double x
  { int x = 3;
    x++;
  }
  { int x = 4; }
*)