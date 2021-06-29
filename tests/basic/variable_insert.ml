open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !!Variable.insert "a" "300" [ tAfter; cTypDef "vect"];
    !!Variable.insert "b" "300" [ tBefore; cVarDef "x"];
)