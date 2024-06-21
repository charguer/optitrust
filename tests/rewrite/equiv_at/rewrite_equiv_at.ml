open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Rewrite_basic.equiv_at "int a, b; int k; ==> a + k * b == b * k + a" [cWriteVar "res"; dRHS];
  !! Rewrite_basic.equiv_at "int a; int k; ==> a + k * a == (k + 1) * a" [cWriteVar "res1"; dRHS];
  !! Rewrite_basic.equiv_at ~indepth:true "int a; int k; ==> a + k * a == (k + 1) * a" [cVarDef "res2"];
  !! Rewrite_basic.equiv_at ~indepth:true "int a; int k; ==> a + k * a == (k + 1) * a" [cVarDef "res4"];
  !! Rewrite_basic.equiv_at ~indepth:true "int k; ==> k + 0 == k" [];
  !! Rewrite_basic.equiv_at ~indepth:true " ==> 8 + 1 == 9" [];
  !! Rewrite_basic.equiv_at ~indepth:true ~ctx:true " ==> min(0, 1) == 0" [];
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Rewrite_basic.equiv_at ~indepth:true " ==> 7 + 1 == 8" []);
)
