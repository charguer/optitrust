open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Matrix.delocalize ~var:"a" ~local_var:"x" ~dim:(trm_var "N0") ~index:"i0" ~acc:"sum" ~ops:(Delocalize_arith (Lit_int 0, Binop_add)) [cFor "i"];
)