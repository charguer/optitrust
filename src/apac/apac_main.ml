open Target
open Ast
open Trm

(** [compile]: applies the compilation chain of the Automatic PArallelizer for C
    on the current abstract syntax tree. *)
let compile () : unit =
  bigstep "Pre-processing";
  !? "Explode multiple variable declarations"
    Apac_preprocessing.explode_let_mult [
      nbAny;
      cVarsDef ""
    ];
  !? "Build global variable records"
    Apac_preprocessing.record_globals [
      nbAny;
      cStrict;
      cVarDef ""
    ];
  !? "Build sequential function implementation records"
    Apac_preprocessing.record_sequentials [
      nbAny;
      cFunDefAndDecl ~regexp:true
        (Str.global_replace (Str.regexp "%f") ".*" !Apac_flags.sequential)
    ];
  let selector : constr =
    if !Apac_flags.omit <> "" then
      cDiff
        [[cFunDefAndDecl ""]]
        [[cFunDefAndDecl ~regexp:true !Apac_flags.omit]]
    else
      cFunDefAndDecl ""
  in
  !? "Build function records"
    Apac_preprocessing.record_functions [
      nbAny;
      selector
    ];
  if !Apac_flags.constify then
    !? "Constify"
      (Apac_preprocessing.Constification.constify
         ~frs:(Some Apac_records.functions)
         ~trans:(not !Apac_flags.constify_quietly)) [
        nbAny;
        selector
      ];
  !? "Select taskification targets"
    Apac_preprocessing.select_candidates [
      nbAny;
      selector
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
  !? "Normalize statement bodies"
    Apac_preprocessing.normalize_statement_bodies [
      nbAny;
      cMark Apac_macros.candidate_body_mark;
      cOr [[cFor ""]; [cFor_c ""]; [cWhile ()]; [cDoWhile ()]; [cIf ()]]
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
  !? "Disqualify taskification targets without taskifiable task candidates"
    Apac_task_candidate_discovery.disqualify_candidates [
      nbAny;
      cMark Apac_macros.candidate_mark
    ];
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
  if !Apac_flags.cutoff_count || !Apac_flags.cutoff_depth then
    !? "Cut off according to task count and/or depth"
      Apac_parallelization.cutoff_count_and_depth [
        nbAny;
        cOr [[cMark Apac_macros.candidate_mark];
             [cFunDefAndDecl !Apac_flags.main]]
      ];
  if !Apac_flags.profile then
    !? "Cut off according to execution time model"
      Apac_parallelization.cutoff_execution_time ();
  !? "Output parallel source code"
    Apac_parallelization.clear_marks ()
