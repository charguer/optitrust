open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
        show [cFor "i"];
        !! Loop.color "C" "2" [cFor "i"] ;
        !! Loop.color "C" "D" [cFor "j"] ;
)
