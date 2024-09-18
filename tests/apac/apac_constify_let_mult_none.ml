open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            !! Apac_preprocessing.Constification.constify_let_mult [
                nbMulti;
                cVarsDef ""
              ];
          );
        Apac_reset.tnt_blast ()
