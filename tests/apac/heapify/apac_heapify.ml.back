open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            (* Target all of the simple variable declarations except for the
               declarations of [i] and [x] (used only for testing purposes). *)
            !! Apac_basic.heapify [
                nbAny;
                cDiff [[cDiff [[cVarDef ""]] [[cVarDef "i"]]]] [[cVarDef "x"]]
              ];
            (* Target all of the multiple variable declarations. *)
            !! Apac_basic.heapify [nbAny; cVarsDef "" ];
          )
