open Optitrust
open Target
open Typ
open Ast

let _ = Run.script_cpp (fun () ->
            Flags.code_print_width := 1024;
            Apac_flags.instrument := false;
            Apac_flags.keep_graphs := true;
            !! Apac_prologue.build_records [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_prologue.select_candidates [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_prologue.unify_returns [
                nbAny;
                cMark Apac_macros.candidate_mark
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
            ?? (fun () ->
                !! Apac_backend.profile_tasks [
                    nbAny;
                    cMark Apac_macros.candidate_body_mark
                  ];
                !! Apac_epilogue.clear_marks ();
                !! Apac_profiler.modelize []
              );
            !! Apac_profiler.optimize [
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
            !! Apac_epilogue.heapify [
                nbAny;
                cOr [[cMark Apac_macros.heapify_mark];
                     [cMark Apac_macros.heapify_breakable_mark]]
              ];
            !! Apac_epilogue.dynamic_cutoff [];
            !! Apac_epilogue.clear_marks ()
          )
