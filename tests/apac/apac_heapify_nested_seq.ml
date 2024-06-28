open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            (* Target all statement sequences enclosed in curly brackets. *)
            !! Apac_epilogue.heapify [nbAny; cSeq ()];
)
