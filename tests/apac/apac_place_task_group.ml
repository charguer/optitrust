open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
            !! Apac_preprocessing.unify_returns [
                nbAny; cFunDefAndDecl ""
              ];
            !! Apac_parallelization.place_task_group [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_parallelization.clear_marks ()
          );
        Apac_reset.tnt_blast ()
