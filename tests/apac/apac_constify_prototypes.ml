open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            !! Apac_constification.constify_prototypes
              [nbMulti; cTopFunDefAndDecl "" ];
          );
        Apac_reset.tnt_blast ()
