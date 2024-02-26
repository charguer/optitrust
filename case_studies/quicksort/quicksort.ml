open Optitrust
open Target
open Typ (* TODO: avoid this *)
open Ast

let _ = Run.script_cpp (fun () ->
            (* Target all of the function definitions except for the 'main'
               function. *)
            !! Apac.constify [
                nbAny;
                cDiff [[cFunDefAndDecl ""]] [[cFunDefAndDecl "main"]]
              ];
            (* Target all of the function definitions. *)
            !! Apac.parallel_task_group ~mark_group:true [
                nbAny;
                cFunDefAndDecl "sort_core"
              ];
            (* Target the main function's definition. *)
            !! Apac.parallel_task_group [
                nbAny;
                cFunDefAndDecl "main"
              ];
            !! Apac_core.taskify [nbAny; cMark Apac_core.task_group_mark];
            !! Apac_core.merge [nbAny; cMark Apac_core.task_group_mark];
            !! Apac_core.insert_tasks [nbAny; cMark Apac_core.task_group_mark];
            !! Marks.remove Apac_core.task_group_mark [
                nbAny;
                cMark Apac_core.task_group_mark
              ];
            !! Apac_basic.heapify [
                nbAny;
                cOr [[cMark Apac_core.heapify_mark];
                     [cMark Apac_core.heapify_breakable_mark]]
              ];
            !! Marks.remove Apac_core.heapify_mark [
                nbAny;
                cMark Apac_core.heapify_mark
              ];
            !! Marks.remove Apac_core.heapify_breakable_mark [
                nbAny;
                cMark Apac_core.heapify_breakable_mark
              ];
          (* !! Apac_core.taskify [nbAny; cMark Apac_core.task_group_mark];
             !! Apac_core.profile_tasks [nbAny; cMark Apac_core.task_group_mark];
             !! Apac_core.include_apac_profiler []; *)
          )
