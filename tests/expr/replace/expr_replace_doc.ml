open Optitrust
open Target
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! Expr_basic.replace (lit "24") [sExpr "20"];

)
