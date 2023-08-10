open Optitrust
open Target 

let _ = Run.script_cpp (fun () -> 

  !! Apac_basic.const_lookup_candidates [nbAny; cFunDefAndDecl ""];

  !! Apac_basic.const_compute_all [nbAny; cFunDefAndDecl ""];

  Apac_basic.unconst_and_const ();
  
  !! Apac.constify_functions_arguments Apac_basic.cstfbl [nbAny; cFunDefAndDecl ""];

)
