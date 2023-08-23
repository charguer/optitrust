open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            !! Apac_basic.mark_taskification_candidates [nbMulti; cFunDef "" ];
          )
