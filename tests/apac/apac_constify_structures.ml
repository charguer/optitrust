open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
            !! Apac_preprocessing.Constification.constify [
                nbAny;
                cFunDefAndDecl ""
              ];
          );
        Apac_reset.tnt_blast ()
