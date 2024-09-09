open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  let fv n = find_var n [] in
  (* replace the function call to "f" with a function call to "f1" *)
  !! Expr.replace_fun ~inline:true (fv "f1") [cFun "f"];
  !! Expr.replace_fun (fv "f") [cFun "f1"];
  !! Expr.replace_fun ~inline:true (fv "f3") [cFun "f2"];
  !! Expr.replace_fun (fv "f2") [cFun "f3"];

)
