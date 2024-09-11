open Optitrust
open Target
open Typ
open Ast

let _ = Run.script_cpp (fun () ->
            let _ = Flags.code_print_width := 1024 in
            let _ = Apac_macros.instrument_code := false in
            let _ = Apac_macros.keep_graphs := true in
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
            !! Apac_backend.profile_tasks [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_epilogue.clear_marks ();
            !! Apac_profiler.modelize [];
            (* !! Apac_epilogue.synchronize_subscripts [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Apac_epilogue.place_barriers [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Apac_taskify.insert_tasks
              [nbAny; cMark Apac_macros.task_group_mark];
          (*  !! Apac_epilogue.instrument
              []
              [nbAny; cMark Apac_macros.task_group_mark]; *)
            !! Marks.remove Apac_macros.task_group_mark [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Apac_epilogue.heapify [
                nbAny;
                cOr [[cMark Apac_macros.heapify_mark];
                     [cMark Apac_macros.heapify_breakable_mark]]
              ];
            !! Marks.remove Apac_macros.heapify_mark [
                nbAny;
                cMark Apac_macros.heapify_mark
              ];
            !! Marks.remove Apac_macros.heapify_breakable_mark [
                nbAny;
                cMark Apac_macros.heapify_breakable_mark
              ]; *)
          )
