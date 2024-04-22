open Optitrust
open Target
open Typ (* TODO: avoid this *)
open Ast

let _ = Run.script_cpp (fun () ->
            let _ = Flags.code_print_width := 1024 in
            let _ = Apac_macros.instrument_code := false in
            let _ = Apac_macros.apac_main := "main" in
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
            !! Apac_epilogue.reduce_waits [
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
          (* !! Apac_core.taskify [nbAny; cMark Apac_core.task_group_mark];
             !! Apac_core.profile_tasks [nbAny; cMark Apac_core.task_group_mark];
             !! Apac_core.include_apac_profiler []; *)
          )
