open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    
    !! Function.inline ~name_result:"r" [cFun "g"; cFun "f"];
    !! Function.inline ~name_result:"r" [cVarDef "p"; cFun "f"];
)
