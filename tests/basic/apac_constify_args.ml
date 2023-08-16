open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            !! Apac.constify_args ~force:true [nbMulti; cTopFunDefAndDecl "" ];
          )
