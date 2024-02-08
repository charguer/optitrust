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
                cFunDefAndDecl ""
              ];
            (* Perform cleaning. *)
            !! Marks.remove Apac_core.task_group_mark [
                nbAny;
                cMark Apac_core.task_group_mark
              ];
          (* !! Apac_core.taskify [nbAny; cMark Apac_core.task_group_mark];
             !! Apac_core.profile_tasks [nbAny; cMark Apac_core.task_group_mark];
             !! Apac_core.include_apac_profiler []; *)
          )
