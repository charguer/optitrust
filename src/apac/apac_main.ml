open Target
open Ast
open Trm

(** [compile]: applies the compilation chain of the Automatic PArallelizer for C
    on the current abstract syntax tree. *)
let compile () : unit =
  Nobrace.enter ();
  bigstep "Pre-processing";
  !? "Build global variable records"
    Apac_preprocessing.record_globals [
      nbAny;
      cStrict;
      cVarDef ""
    ];
  !? "Build function records"
    Apac_preprocessing.record_functions [
      nbAny;
      cFunDefAndDecl ""
    ];
  if !Apac_flags.constify then
    !? "Constify"
      (Apac_preprocessing.Constification.constify
      ~frs:(Some Apac_records.functions)) [
        nbAny;
        cFunDefAndDecl ""
      ];
  !? "Select taskification targets"
    Apac_preprocessing.select_candidates [
      nbAny;
      cFunDefAndDecl ""
    ];
  !? "Unify returns"
    Apac_preprocessing.unify_returns [
      nbAny;
      cOr [[cMark Apac_macros.candidate_mark];
           [cFunDefAndDecl !Apac_flags.main]]
    ];
  !? "Unfold function calls"
    Apac_preprocessing.unfold_function_calls [
      nbAny;
      cMark Apac_macros.candidate_body_mark;
      cFun "";
    ];
  !? "Detach function calls"
    Apac_preprocessing.detach_function_calls [
      nbAny;
      cMark Apac_macros.candidate_body_mark;
      cVarDef ""
    ];
  bigstep "Task candidate discovery";
  !? "Translate into task candidate representation"
    Apac_task_candidate_discovery.taskify [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  !? "Merge task candidates"
    Apac_task_candidate_discovery.merge [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  !? "Select eligible task candidates"
    Apac_task_candidate_discovery.detect_tasks_simple [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  if !Apac_flags.profile then
    begin
      bigstep "Execution time modeling";
      ?? "Roll-back profiling transformations" (fun () ->
          !? "Annotate with profiling instructions"
            Apac_profiling.annotate [
              nbAny;
              cMark Apac_macros.candidate_body_mark
            ];
          !? "Output profiling source code"
            Apac_parallelization.clear_marks ();
          !? "Compile, run and compute model"
            Apac_profiling.modelize []
        );
      !? "Refine the selection of eligible task candidates"
        Apac_profiling.optimize [
          nbAny;
          cMark Apac_macros.candidate_body_mark
        ]
    end;
  bigstep "Parallel source code generation";
  !? "Synchronize array subscripts"
    Apac_parallelization.synchronize_subscripts [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  !? "Place barriers"
    Apac_parallelization.place_barriers [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  !? "Place tasks"
    Apac_parallelization.insert_tasks [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  !? "Place task groups"
    Apac_parallelization.place_task_group [
      nbAny;
      cOr [[cMark Apac_macros.candidate_body_mark];
           [cMark Apac_macros.candidate_main_mark]]
    ];
  !? "Heapify"
    Apac_parallelization.heapify [
      nbAny;
      cOr [[cMark Apac_macros.heapify_mark];
           [cMark Apac_macros.heapify_breakable_mark]]
    ];
  !? "Secure accesses to global variables"
    Apac_parallelization.secure_globals [
      nbAny;
      cFunDefAndDecl "";
      dBody
    ];
  if !Apac_flags.cutoff_count_and_depth then
    !? "Cut off according to task count and depth"
      Apac_parallelization.cutoff_count_and_depth [
        nbAny;
        cMark Apac_macros.candidate_body_mark
      ];
  if !Apac_flags.profile then
    !? "Cut off according to execution time model"
      Apac_parallelization.cutoff_execution_time ();
  !? "Output parallel source code"
    Apac_parallelization.clear_marks ()

