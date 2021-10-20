open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

    show [cFor "i"];
    !! Matrix_basic.local_other_name ~var:"a" ~local_var:"x" [cFor "i"];

)