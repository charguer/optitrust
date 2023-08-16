open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            let target = [nbAny; cFunDefAndDecl ""] in
            !! Apac_basic.const_lookup_candidates target;
            !! Apac_basic.const_compute_all target;
            !! Apac_basic.unconst ();
            !! Apac_basic.constify_args target;
            !! Apac_basic.constify_aliases target;
)
