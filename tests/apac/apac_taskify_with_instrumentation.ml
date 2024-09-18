open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            Flags.code_print_width := 1024;
            Apac_flags.instrument := true;
            !! Apac_prologue.build_records [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_preprocessing.Constification.constify
              ~frs:(Some Apac_records.functions) [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_preprocessing.unify_returns [
                nbAny;
                cFunDefAndDecl "c"
              ];
            !! Apac_task_candidate_discovery.taskify [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_task_candidate_discovery.merge [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_task_candidate_discovery.detect_tasks_simple [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_epilogue.synchronize_subscripts [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_epilogue.place_barriers [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ]; 
            !! Apac_backend.insert_tasks [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_epilogue.place_task_group [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_epilogue.instrument [] [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_epilogue.clear_marks ()
          );
        Apac_reset.tnt_blast ()
