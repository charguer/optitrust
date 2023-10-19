open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->
  let fv = find_var_in_current_ast in
  (* replace the function call to "f" with a function call to "f1" *)
  !! Expr_basic.replace_fun (fv "f1") [cFun "f"];
  !! Expr_basic.replace_fun (fv "f") [occIndex ~nb:2 1; cFun "f1"];
  !! Expr_basic.replace_fun (fv "f3") [cFun "f2"];
  !! Expr_basic.replace_fun (fv "f2") [occIndex ~nb:2 1; cFun "f3"];

)
