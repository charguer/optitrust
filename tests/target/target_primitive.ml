open Optitrust
open Prelude
let show = Show.add_marks_for_target_unit_tests


let _ = Run.script_cpp (fun _ ->

  !! show [cPrimCall (Prim_binop Binop_add)];
  !! show [cPrimCall (Prim_binop Binop_mul)];
  !! show [cPrimCall (Prim_binop Binop_exact_div)];
  !! show [cPrimCall (Prim_binop Binop_trunc_div)];
  !! show [cPrimCall (Prim_binop Binop_trunc_mod)];
  !! show [cDiv ()];
)
