open Optitrust
open Target

(* TODO : Add a doc ?  *)
let _ = Run.script_cpp (fun () ->

   !! Function.uninline ~f:[cTopFunDef "f"] [cTopFunDef "main"; cVarDef "a"];

)
