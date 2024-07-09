open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! Expr_basic.replace ~reparse:true (lit "24") [sExpr "20"];

)
