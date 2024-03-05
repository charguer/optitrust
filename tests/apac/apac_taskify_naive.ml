open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            (* Target all of the function definitions. *)
            !! Apac_constify.constify [nbAny; cFunDefAndDecl ""];
            !! Apac_taskify.parallel_task_group
              ~mark_group:true [nbAny; cFunDef "h"];
            !! Apac_taskify.taskify [nbAny; cMark Apac_macros.task_group_mark];
            !! Apac_taskify.merge [nbAny; cMark Apac_macros.task_group_mark];
            !! Apac_taskify.insert_tasks
              [nbAny; cMark Apac_macros.task_group_mark];
          )
