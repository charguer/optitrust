open Optitrust
open Target

let () =
  let header = Apac_macros.cwd () ^ "/tests/apac/apac_profiling.hpp" in
  Apac_macros.profile_hpp "/dev/null" header;
  Run.script_cpp ~check_syntax_at_end:true (fun () ->
      !! Apac_preprocessing.record_functions [
          nbAny;
          cFunDefAndDecl ""
        ];
      !! Apac_preprocessing.Constification.constify
        ~frs:(Some Apac_records.functions) [
          nbAny;
          cFunDefAndDecl ""
        ];
      !! Marks_basic.add Apac_macros.candidate_mark [
          nbAny;
          cFunDefAndDecl "c"
        ];
      !! Apac_preprocessing.unify_returns [
          nbAny;
          cMark Apac_macros.candidate_mark
        ];
      !! Apac_task_candidate_discovery.taskify [
          nbAny;
          cMark Apac_macros.candidate_body_mark
        ];
      !! Apac_task_candidate_discovery.detect_tasks_simple [
          nbAny;
          cMark Apac_macros.candidate_body_mark
        ];
      !! Apac_profiling.annotate [
          nbAny;
          cMark Apac_macros.candidate_body_mark
        ];
      !! Apac_profiling.annotate_main [
          nbAny;
          cFunBody !Apac_flags.main
        ];
      !! Apac_parallelization.clear_marks ();
    );
  Apac_reset.tnt_blast ();
  Unix.unlink header
