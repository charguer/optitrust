open Optitrust
open Target
open Prelude

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
   show [cRead ~addr:[sExpr "a"] ()];
   show [cRead ~addr:[cVar "b"] ()];
   show [cRead ~addr:[cVar "i"] ()];



)
