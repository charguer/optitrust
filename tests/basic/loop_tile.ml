open Optitrust
open Target

(* Works *)
let _ = Run.script_cpp (fun _ -> 
        Loop.tile "2" "bx" [cFor "x"] ;
    )