open Optitrust
open Target

(* Works *)
let _ = Run.script_cpp ( fun _ ->
        Loop.split 1 [cFor "i"];
)
