open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            !! Apac.mark_taskification_candidates [nbMulti; cFunDef "" ];
          )
