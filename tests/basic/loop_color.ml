open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
        !! Loop.color "C" "2" [cForSimple "i"] ;
        !! Loop.color "C" "D" [cForSimple "j"] ;
)
