open Optitrust
open Target
open Typ
open Ast

let _ = Run.script_cpp (fun () ->
            let _ = Apac_macros.keep_graphs := true in
            !! Apac_prologue.build_records [
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
              ~mark_group:true ~placeholder:true ~candidates:(Some candidates) [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_taskify.taskify [nbAny; cMark Apac_macros.task_group_mark];
            !! Apac_taskify.merge [nbAny; cMark Apac_macros.task_group_mark];
            !! Apac_taskify.detect_tasks_simple [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Apac_taskify.profile_tasks
              [nbAny; cMark Apac_macros.task_group_mark];
            !! Marks.remove Apac_macros.task_group_mark [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
          )
