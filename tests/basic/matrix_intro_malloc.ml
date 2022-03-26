open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

    !! Matrix_basic.intro_malloc [cVarDef "p"; cFun "malloc"];
)