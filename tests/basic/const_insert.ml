open Optitrust
open Target

let _ = Run.script_cpp
    ( fun _ -> 
        Variable.insert ~const:true  "NB_VECTS" "100" [tAfter;cTypDef "vect"];
    )