open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

    !! Matrix.intro_malloc [cVarDef "p"];
    !! Matrix.intro_malloc [cVarDef "q"];

)
