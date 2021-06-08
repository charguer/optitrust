open Optitrust
open Run

(* TODO: References aren't yet implemented*)

let _ = run_unit_test 
    ( fun _-> 
    Declaration.fold [cSet ~lhs:[cVar ~name:"y" ()] ()] ();
    Declaration.fold [cSet ~lhs:[cVar ~name:"a" ()] ()] ();
    Declaration.fold [cSet ~lhs:[cVar ~name:"b" ()] ()] ();
    Declaration.fold [cSet ~lhs:[cVar ~name:"v" ()] ()] ();
)