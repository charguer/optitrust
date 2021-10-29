open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    !! Matrix_basic.local_name ~var:"a" ~local_var:"x" [cFor "i"];

)