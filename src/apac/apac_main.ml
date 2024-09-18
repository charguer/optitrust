open Target

(** [compile]: applies the compilation chain of the Automatic PArallelizer for C
    on the current abstract syntax tree. *)
let compile () : unit =
  !! Apac_prologue.build_records [nbAny; cFunDefAndDecl ""];
  if !Apac_flags.constify then
    !! Apac_constification.constify ~frs:(Some Apac_records.functions) [
        nbAny;
        cFunDefAndDecl ""
      ];
  !! Apac_prologue.select_candidates [nbAny; cFunDefAndDecl ""];
  !! Apac_prologue.unify_returns [nbAny; cMark Apac_macros.candidate_mark];
  !! Apac_taskify.taskify [nbAny; cMark Apac_macros.candidate_body_mark];
  !! Apac_taskify.merge [nbAny; cMark Apac_macros.candidate_body_mark];
  !! Apac_taskify.detect_tasks_simple [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  if !Apac_flags.profile then
    begin
      ?? (fun () ->
          !! Apac_profiler.annotate [
              nbAny;
              cMark Apac_macros.candidate_body_mark
            ];
          !! Apac_epilogue.clear_marks ();
          !! Apac_profiler.modelize []
        );
      !! Apac_profiler.optimize [nbAny; cMark Apac_macros.candidate_body_mark];
    end;
  !! Apac_epilogue.synchronize_subscripts [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  !! Apac_epilogue.place_barriers [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  !! Apac_backend.insert_tasks [nbAny; cMark Apac_macros.candidate_body_mark];
  !! Apac_epilogue.place_task_group [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  !! Apac_epilogue.heapify [
      nbAny;
      cOr [[cMark Apac_macros.heapify_mark];
           [cMark Apac_macros.heapify_breakable_mark]]
    ];
  if !Apac_flags.profile then
    !! Apac_epilogue.execution_time_cutoff [];
  !! Apac_epilogue.clear_marks ()
              
