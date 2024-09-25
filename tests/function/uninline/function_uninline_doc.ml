open Optitrust
open Target


let _ = Run.script_cpp (fun () ->

   !! Function.uninline ~f:[cTopFunDef "f"] [cTopFunDef "main"; cVarDef "a"];

)
