open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    !! Matrix_basic.local_name  "a" ~into:"x" [cFor "i"];
)
