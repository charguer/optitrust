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
            !! Apac_prologue.select_candidates [
                nbAny;
                cFunDefAndDecl ""
              ];
            !! Apac_prologue.unify_returns [
                nbAny;
                cMark Apac_macros.candidate_mark
              ];
            !! Apac_taskify.taskify [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_taskify.merge [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_taskify.detect_tasks_simple [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_taskify.profile_tasks [
                nbAny;
                cMark Apac_macros.candidate_body_mark
              ];
            !! Apac_epilogue.clear_marks ()
          )
