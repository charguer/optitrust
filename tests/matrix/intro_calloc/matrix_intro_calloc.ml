open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

    !! Matrix.intro_calloc [cVarDef "p"];
    !! Matrix.intro_calloc [cVarDef "q"];

)
