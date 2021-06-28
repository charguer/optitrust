open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
        !! Loop.swap [cForSimple "a"];
        !! Loop.swap [cForSimple "b"];
)
