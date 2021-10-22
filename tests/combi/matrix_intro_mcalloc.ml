open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

    !! Matrix.intro_mcalloc [cVarDef "p"];
)