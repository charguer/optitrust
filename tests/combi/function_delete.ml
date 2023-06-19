open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   !! Function.delete [cFunDef "f"];
   !! Function.delete [cFunDef "g"];
   !!! ();

)
