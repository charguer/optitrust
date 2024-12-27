open Optitrust
open Target

let _ = Run.script_cpp ~check_syntax_at_end:true (fun _ ->
            !! Apac_preprocessing.unify_returns [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_parallelization.clear_marks ()
          );
        Apac_reset.tnt_blast ()
