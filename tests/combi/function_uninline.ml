open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

    !! Function.uninline ~fct:[cTopFunDef "f"] [cVarDef "b"];

)
