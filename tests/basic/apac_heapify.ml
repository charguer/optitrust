open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            (* Target the body of the main function. *)
            !! Apac_basic.heapify [cFunBody "main"];
          )
