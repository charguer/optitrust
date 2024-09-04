open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
            !! Apac_prologue.unify_returns [
                nbAny; cFunDefAndDecl ""
              ];
            !! Apac_epilogue.place_task_group [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_epilogue.clear_marks ()
          );
        Apac_reset.tnt_blast ()
