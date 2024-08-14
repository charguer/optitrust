open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            let _ = Flags.code_print_width := 1024 in
            (* Target all of the function definitions except for the 'main'
               function. *)
            !! Apac_constify.constify [
                nbAny;
                cFunDefAndDecl ""
              ];
            (* Target the definition of the 'sort_core' function. *)
            !! Apac_taskify.parallel_task_group ~mark_group:true [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_taskify.taskify [nbAny; cMark Apac_macros.task_group_mark];
            !! Apac_taskify.find_candidates_minimum_funcalls ~min:2
              [nbAny; cMark Apac_macros.task_group_mark];
            !! Apac_taskify.taskify_callers ();
            !! Apac_taskify.restore [nbAny; cFunDefAndDecl ""];
            !! Apac_taskify.merge [nbAny; cMark Apac_macros.task_group_mark];
            !! Apac_taskify.detect_tasks_simple [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Apac_epilogue.synchronize_subscripts [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
          (*  !! Apac_epilogue.reduce_waits [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];  *)
            !! Apac_epilogue.place_barriers [
                nbAny;
                cMark Apac_macros.task_group_mark
              ]; 
            !! Apac_taskify.insert_tasks
              [nbAny; cMark Apac_macros.task_group_mark];
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
