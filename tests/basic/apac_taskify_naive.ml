open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            (* Target all of the function definitions. *)
            !! Apac.constify [nbAny; cFunDefAndDecl ""];
            !! Apac.parallel_task_group [nbAny; cFunDef "h"];
            !! Apac_core.taskify [nbAny; cFunDef ""];
          )
