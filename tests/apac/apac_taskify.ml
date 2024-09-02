open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun () ->
            let _ = Flags.code_print_width := 1024 in
            !! Apac_prologue.build_records [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_constification.constify
              ~frs:(Some Apac_records.functions) [
                nbAny;
                cFunDefAndDecl ""
              ];
            let candidates = ref Var_set.empty in
            Apac_taskify.select_candidates candidates [
                nbAny;
                cFunDefAndDecl ""
              ];
            Apac_taskify.select_callers candidates [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_taskify.parallel_task_group
              ~mark_group:true ~candidates:(Some candidates) [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_taskify.taskify [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Apac_taskify.merge [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Apac_taskify.detect_tasks_simple [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Apac_epilogue.synchronize_subscripts [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Apac_epilogue.place_barriers [
                nbAny;
                cMark Apac_macros.task_group_mark
              ]; 
            !! Apac_taskify.insert_tasks [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
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
              ];
          );
        Apac_reset.tnt_blast ()
