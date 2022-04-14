open Optitrust
open Target
open Ast

let _ = Run.doc_script_cpp (fun _ ->
    !! Accesses.shift (trm_double 5.0) [cReadVar "x"]
  )
"
int main() {
  int x = 2;
  int y = x;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Accesses.shift (trm_double 5.0) [nbMulti; cTopFunDef "test_var"; cVar "x"];
  !! Accesses.shift (trm_double 5.0) [nbMulti; cTopFunDef "test_array"; sExpr "t[0]"];
  !! Accesses.shift (trm_double 5.0) [nbMulti; cTopFunDef "main"; sExpr "t[i]"];
)
