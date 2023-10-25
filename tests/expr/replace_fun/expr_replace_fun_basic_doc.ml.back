open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->
  let g = find_var_in_current_ast "g" in
  !! Expr_basic.replace_fun g [cFun "f"];

)
