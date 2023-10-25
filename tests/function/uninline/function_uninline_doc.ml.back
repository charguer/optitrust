open Optitrust
open Target


let _ = Run.script_cpp (fun () ->

   !! Function.uninline ~fct:[cTopFunDef "f"] [cTopFunDef "main"; cVarDef "a"];

)
