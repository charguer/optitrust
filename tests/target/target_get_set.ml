open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun () ->
  
   (* All right hand sides of the equalities*)
   show [dRHS];
   (* All left hand sides of the equalities*)
   show [dLHS];   
   (* All equalities*)
   show [cWrite ()];
   (* Equalities with specific right hand side*)
   show [cWrite ~lhs:[cVar "i"] ()];
   show [cWrite ~lhs:[cVar "t"] ()];
   (* All get operations *)
   show [cRead ~arg:[sExpr "a"] ()];
   show [cRead ~arg:[cVar "b"] ()];
   show [cRead ~arg:[cVar "i"] ()];
   
   
   
)
