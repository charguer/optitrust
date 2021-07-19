open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Sequence.insert "int a = 300;" [ tAfter; cTypDef "vect"];
    !! Sequence.insert "int b = 500;" [ tAfter; cVarDef "x"];
)
