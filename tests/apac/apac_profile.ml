open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            !! Apac_prologue.build_records [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_constification.constify
              ~frs:(Some Apac_records.functions) [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_taskify.parallel_task_group ~mark_group:true
              ~placeholder:true [
                nbAny;
                cFunDef "c"
              ];
            !! Apac_taskify.taskify [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Apac_taskify.profile_tasks [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Marks.remove Apac_macros.heapify_breakable_mark [
                nbAny;
                cMark Apac_macros.heapify_breakable_mark
              ];
          );
        Apac_reset.tnt_blast ()
