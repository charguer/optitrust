open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            !! Apac_preprocessing.record_functions [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_preprocessing.Constification.constify
              ~frs:(Some Apac_records.functions) [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Marks_basic.add Apac_macros.candidate_mark [
                nbAny;
                cFunDefAndDecl "c"
              ];
            !! Apac_preprocessing.unify_returns [
                nbAny;
                cMark Apac_macros.candidate_mark
              ];
            !! Apac_task_candidate_discovery.taskify [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_task_candidate_discovery.detect_tasks_simple [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_profiling.annotate [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_parallelization.clear_marks ();
          );
        Apac_reset.tnt_blast ()
