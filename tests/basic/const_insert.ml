open Optitrust
open Target

let _ = Run.script_cpp ( fun _ -> 
        (* TODO: More variable insertions here *)
        !! Variable.insert ~const:true  "NB_VECTS" "100" [tAfter;cTypDef "vect"];
)
