open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->
  let fv n = fst (find_var n []) in
  (* replace the function call to "f" with a function call to "f1" *)
  !! Expr_basic.replace_fun (fv "f1") [cCall "f"];
  !! Expr_basic.replace_fun (fv "f") [occIndex ~nb:2 1; cCall "f1"];
  !! Expr_basic.replace_fun (fv "f3") [cCall "f2"];
  !! Expr_basic.replace_fun (fv "f2") [occIndex ~nb:2 1; cCall "f3"];

)
