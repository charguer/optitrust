open Optitrust
open Target 

let _ = Run.script_cpp (fun () -> 
            !! Apac_epilogue.heapify [nbAny; cFunDef "main"];
)
