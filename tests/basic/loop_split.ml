open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
    !! Loop.split 1 [cForSimple "i"];
)
