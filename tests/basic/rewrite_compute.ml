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
(* TODO: create a constraint  cPrimFunArith
    and use it as target in the demo and in one example from the test below *)

let _ = Run.script_cpp (fun _ ->

  !! Rewrite_basic.compute [nbMulti; cPrimFun (Prim_binop Binop_add)];
  !! Rewrite_basic.compute [nbMulti; cPrimFun (Prim_binop Binop_mul)];
  !! Rewrite_basic.compute [nbMulti; cPrimFun (Prim_binop Binop_div)];
  !! Rewrite_basic.compute [nbMulti; cPrimFun (Prim_binop Binop_mod)];
)