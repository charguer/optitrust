open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            !! Apac_constify.constify_args
              ~force:true [nbMulti; cTopFunDefAndDecl "" ];
          )
