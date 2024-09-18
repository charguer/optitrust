open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            (* Target all statement sequences enclosed in curly brackets. *)
            !! Apac_parallelization.heapify [nbAny; cSeq ()];
          );
        Apac_reset.tnt_blast ()
