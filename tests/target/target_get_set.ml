open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun () ->
  
   (* All right hand sides of the equalities*)
   show [dRHS];
   (* All left hand sides of the equalities*)
   show [dLHS];   
   (* All equalities*)
   show [cSet ()];
   (* Equalities with specific right hand side*)
   show [cSet ~lhs:[cVar "i"] ()];
   show [cSet ~lhs:[cVar "t"] ()];
   (* All get operations *)
   show [cGet ()];
)
