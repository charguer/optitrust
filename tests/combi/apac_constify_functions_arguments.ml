open Optitrust
open Target 

let _ = Run.script_cpp (fun () -> 

  !! Apac_basic.const_lookup_candidates [cFunDef ""];

  !! Apac_basic.const_compute_all [cFunDef ""];
  
  !! Apac.constify_functions_arguments Apac_basic.cstfbl [cFunDefAndDecl ""];

)