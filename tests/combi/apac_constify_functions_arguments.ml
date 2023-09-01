open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            !! Apac.constify [nbAny; cFunDefAndDecl ""];
)
