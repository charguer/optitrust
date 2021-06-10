open Optitrust
open Target

(* Works *)
let _ = Run.script_cpp (fun _ ->
        Loop.color "C" "2" [cFor "i"] ;
        Loop.color "C" "D" [cFor "j"] ;
)