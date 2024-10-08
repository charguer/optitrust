open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  let fv n = fst (find_var n []) in
  (* replace the function call to "f" with a function call to "f1" *)
  !! Expr.replace_fun ~inline:true (fv "f1") [cCall "f"];
  !! Expr.replace_fun (fv "f") [cCall "f1"];
  !! Expr.replace_fun ~inline:true (fv "f3") [cCall "f2"];
  !! Expr.replace_fun (fv "f2") [cCall "f3"];

)
