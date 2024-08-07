open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.delocalize ~init_zero:true ~acc_in_place:false ~dim:(var "N0") ~index:"i0" ~acc:"sum" ~ops:(Local_arith (Lit_int 0, Binop_add)) [cLabel "mark"];

  !! Trace.restore_original();
  !! Matrix_basic.delocalize ~dim:(var "N0") ~index:"i0" ~acc:"sum" ~ops:(Local_arith (Lit_int 0, Binop_add)) [cLabel "mark"];

)
