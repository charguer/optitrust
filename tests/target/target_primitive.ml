open Optitrust
open Prelude
let show = Show.add_marks_for_target_unit_tests


let _ = Run.script_cpp (fun _ ->

  !! show [cPrimFun (Prim_binop Binop_add)];
  !! show [cPrimFun (Prim_binop Binop_mul)];
  !! show [cPrimFun (Prim_binop Binop_div)];
  !! show [cPrimFun (Prim_binop Binop_mod)];
)
