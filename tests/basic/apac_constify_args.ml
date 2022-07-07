open Optitrust
open Target 

let _ = Run.script_cpp (fun () -> 

  !! Apac.constify_args [nbMulti; cTopFunDefAndDecl "e" ];

  !! Apac.constify_args [nbMulti; cTopFunDefAndDecl "f" ];

  !! Apac.constify_args [nbMulti; cTopFunDefAndDecl "g" ];

  !! Apac.constify_args [nbMulti; cTopFunDefAndDecl "h" ];
  
  !! Apac.constify_args [nbMulti; cTopFunDefAndDecl "i" ];

)