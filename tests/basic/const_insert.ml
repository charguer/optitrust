open Optitrust
open Target
(* Works *)
let _ = Run.script_cpp
    ( fun _ -> 
        Declaration.insert ~const:true  "NB_VECTS" "100" [cAfter;cTypDef "vect"];
    )