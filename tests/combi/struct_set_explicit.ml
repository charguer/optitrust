open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
    !! Struct.set_explicit [cVarDef "p"];
    !! Struct.set_explicit [cVarDef "b"];
    !! Struct.set_explicit [cVarDef "u"];
)
