open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            let cl : bool list = List.init 10 (Fun.const true) in
            !! Apac_constification.constify_let_mult ~cl:(Some cl)
              [nbAny; cVarsDef "" ];
          );
        Apac_reset.tnt_blast ()
