open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
  let g = Syntax.find_var_in_current_ast "g" in
  !! Expr_basic.replace_fun g [cFun "f"];

)

"
int f(int x) { return (x % 2); }

int g(int x) { return (x & 1); }

int main() {
  int a = 5;
  f(a);
}
"

let _ = Run.script_cpp (fun _ ->
  let fv = Syntax.find_var_in_current_ast in
  (* replace the function call to "f" with a function call to "f1" *)
  !! Expr_basic.replace_fun (fv "f1") [cFun "f"];
  !! Expr_basic.replace_fun (fv "f") [occIndex ~nb:2 1; cFun "f1"];
  !! Expr_basic.replace_fun (fv "f3") [cFun "f2"];
  !! Expr_basic.replace_fun (fv "f2") [occIndex ~nb:2 1; cFun "f3"];

)
