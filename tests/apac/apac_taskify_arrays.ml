open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            let _ = Flags.code_print_width := 1024 in
            !! Apac_prologue.build_records [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_constification.constify
              ~frs:(Some Apac_records.functions) [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_taskify.parallel_task_group ~mark_group:true [
                nbAny;
                cFunDefAndDecl "f"
              ];
            !! Apac_taskify.taskify [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Apac_taskify.merge [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Apac_taskify.detect_tasks_simple [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Apac_epilogue.place_barriers [
                nbAny;
                cMark Apac_macros.task_group_mark
              ]; 
            !! Apac_taskify.insert_tasks [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
            !! Marks.remove Apac_macros.heapify_breakable_mark [
                nbAny;
                cMark Apac_macros.heapify_breakable_mark
              ];
            !! Marks.remove Apac_macros.task_group_mark [
                nbAny;
                cMark Apac_macros.task_group_mark
              ];
          );
        Apac_reset.tnt_blast ()
