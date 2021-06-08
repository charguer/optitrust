open Optitrust
open Run
let _ = run_unit_test
    ( fun _ -> 
        Declaration.insert ~const:true  "NB_VECTS" "100" [cAfter;cTypDef "vect"];
    )