open Optitrust
open Target

(* TODO: References aren't yet implemented*)

let _ = Run.script_cpp 
    ( fun _-> 
    Declaration.fold [cSet ~lhs:[cVar ~name:"y" ()] ()] ();
    Declaration.fold [cSet ~lhs:[cVar ~name:"a" ()] ()] ();
    Declaration.fold [cSet ~lhs:[cVar ~name:"b" ()] ()] ();
    Declaration.fold [cSet ~lhs:[cVar ~name:"v" ()] ()] ();
)