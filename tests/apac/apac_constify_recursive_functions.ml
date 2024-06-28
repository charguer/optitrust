open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            !! Apac_constify.constify [nbAny; cFunDefAndDecl ""];
          );
        Apac_reset.tnt_blast ()
