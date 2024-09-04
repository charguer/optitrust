open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            let _ = Flags.code_print_width := 1024 in
            let _ = Apac_macros.instrument_code := true in
            !! Apac_prologue.build_records [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_constification.constify
              ~frs:(Some Apac_records.functions) [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_prologue.unify_returns [
                nbAny;
                cFunDefAndDecl "c"
              ];
            !! Apac_taskify.taskify [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_taskify.merge [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_taskify.detect_tasks_simple [
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
            !! Apac_taskify.insert_tasks [
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
