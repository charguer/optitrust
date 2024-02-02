open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            (* Target all of the function definitions. *)
            !! Apac.constify [nbAny; cFunDefAndDecl ""];
            !! Apac.parallel_task_group ~mark_group:true [nbAny; cFunDef "c"];
            (*!! Apac.parallel_task_group ~mark_group:true [nbAny; cFunDef "main"];*)
            !! Apac_core.taskify [nbAny; cMark Apac_core.task_group_mark];
            (*!! Apac_core.insert_tasks [nbAny; cMark Apac_core.task_group_mark];*)
          )
