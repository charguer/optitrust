open Optitrust
open Target
open Typ
open Ast

let _ = Run.script_cpp (fun () ->
            let _ = Flags.code_print_width := 1024 in
            let _ = Apac_macros.instrument_code := false in
            let _ = Apac_macros.keep_graphs := true in
            let _ = Apac_macros.apac_main := "main" in
            !! Apac_prologue.build_records [
                nbAny;
                cFunDefAndDecl ""
              ];
            (* Target all of the function definitions except for the 'main'
               function. *)
            !! Apac_constification.constify
              ~frs:(Some Apac_records.functions) ~trans:false [
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
            (* Target the definition of the 'sort_core' function. *)
            !! Apac_taskify.parallel_task_group
              ~mark_group:true ~candidates:(Some candidates) [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_taskify.taskify [nbAny; cMark Apac_macros.task_group_mark];
            !! Apac_taskify.merge [nbAny; cMark Apac_macros.task_group_mark];
            !! Apac_taskify.detect_tasks_simple [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Apac_epilogue.synchronize_subscripts [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
           (* !! Apac_epilogue.reduce_waits [
                nbAny;
                cMark Apac_macros.task_group_mark
              ]; *)
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
              ]; 
          )
