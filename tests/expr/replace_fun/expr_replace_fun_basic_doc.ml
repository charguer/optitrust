open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->
  let (g, _) = find_var "g" [] in
  !! Expr_basic.replace_fun g [cCall "f"];

)
