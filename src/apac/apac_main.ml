open Target

(** [compile]: applies the compilation chain of the Automatic PArallelizer for C
    on the current abstract syntax tree. *)
let compile () : unit =
  !! Apac_preprocessing.build_records [
      nbAny;
      cFunDefAndDecl ""
    ];
  if !Apac_flags.constify then
    !! Apac_preprocessing.Constification.constify
      ~frs:(Some Apac_records.functions) [
        nbAny;
        cFunDefAndDecl ""
      ];
  !! Apac_preprocessing.select_candidates [
      nbAny;
      cFunDefAndDecl ""
    ];
  !! Apac_preprocessing.unify_returns [
      nbAny;
      cMark Apac_macros.candidate_mark
    ];
  !! Apac_task_candidate_discovery.taskify [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  !! Apac_task_candidate_discovery.merge [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  !! Apac_task_candidate_discovery.detect_tasks_simple [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  if !Apac_flags.profile then
    begin
      ?? (fun () ->
          !! Apac_profiling.annotate [
              nbAny;
              cMark Apac_macros.candidate_body_mark
            ];
          !! Apac_parallelization.clear_marks ();
          !! Apac_profiling.modelize []
        );
      !! Apac_profiling.optimize [
          nbAny;
          cMark Apac_macros.candidate_body_mark
        ];
    end;
  !! Apac_parallelization.synchronize_subscripts [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  !! Apac_parallelization.place_barriers [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  !! Apac_parallelization.insert_tasks [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  !! Apac_parallelization.place_task_group [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  !! Apac_parallelization.heapify [
      nbAny;
      cOr [[cMark Apac_macros.heapify_mark];
           [cMark Apac_macros.heapify_breakable_mark]]
    ];
  if !Apac_flags.profile then
    !! Apac_parallelization.execution_time_cutoff [];
  !! Apac_parallelization.clear_marks ()
              
