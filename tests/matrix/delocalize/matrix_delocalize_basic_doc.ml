open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.delocalize ~dim:(trm_toplevel_var "N2") ~index:"i2" ~acc:"sum" ~ops:(Local_arith (Lit_int 0, Binop_add)) [cLabel "mark"];

)
