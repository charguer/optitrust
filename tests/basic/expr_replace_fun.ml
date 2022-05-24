open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Expr_basic.replace_fun "g" [cFun "f"];
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

  (* replace the function call to "f" with a function call to "f1" *)
  !! Expr_basic.replace_fun "f1" [cFun "f"];
  !! Expr_basic.replace_fun "f" [occIndex ~nb:2 1; cFun "f1"];
  !! Expr_basic.replace_fun "f3" [cFun "f2"];
  !! Expr_basic.replace_fun "f2" [occIndex ~nb:2 1; cFun "f3"];

)
