open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            let deletes : Apac_core.trmq = Queue.create () in
            let deletes = Some (deletes) in
            (* Target all of the simple variable declarations except for the
               declarations of [i] and [x] (used only for testing purposes). *)
            !! Apac_basic.heapify_intro ~deletes [
                nbAny;
                cDiff [[cDiff [[cVarDef ""]] [[cVarDef "i"]]]] [[cVarDef "x"]]
              ];
            (* Target all of the multiple variable declarations. *)
            !! Apac_basic.heapify_intro ~deletes [nbAny; cVarsDef "" ];
            !! Apac_basic.heapify_outro ~deletes [cFunBody "main"];
          )
