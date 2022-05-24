open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

  !! Rewrite_basic.compute [cPrimFun (Prim_binop Binop_add)];
)

"
int main() {
  int a = (6 + 2);
}
"

let _ = Run.script_cpp (fun _ ->

  !! Rewrite_basic.compute [nbMulti; cPrimFunArith ()];

)
