open Optitrust
open Target



(* TODO: Fix the issue with cInit *)
let _ = Run.doc_script_cpp (fun _ ->
  !! Rewrite_basic.equiv_at "int x, k, l; ==> k + x * l == l * x + k" [cVarDef "b"; cInit()];
  )
"
int main() {
  int a = 2;
  int b = 3 + a * 4;
}
"


let _ = Run.script_cpp (fun _ ->

  !! Rewrite_basic.equiv_at "double a, b; int k; ==> a + k * b == b * k  + a" [cWriteVar "res"; dRHS];
  !! Rewrite_basic.equiv_at "double a; int k; ==> a + k * a == (k + 1) * a" [cWriteVar "res1"; dRHS];
  !! Rewrite_basic.equiv_at "double a; int k; ==> a + k * a == (k + 1) * a" [cVarDef "res2"; dBody];
)
