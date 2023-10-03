open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            (* Target all of the function definitions. *)
            !! Apac.constify [nbAny; cFunDefAndDecl ""];
            !! Apac_basic.task_group [nbAny; cFunDef "c"];
            !! Apac_basic.use_goto_for_return ~mark:Apac_core.task_group_mark [nbAny; cFunDef "main"];
            !! Apac_basic.task_group ~master:true [nbAny; cFunDef "main"];
            !! Apac_core.taskify [nbAny; cFunDef ""];
          )
