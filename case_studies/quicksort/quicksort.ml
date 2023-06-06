open Optitrust
open Target


let _ = Run.script_cpp (fun () ->
  let t = get_ast() in
  Ast_data.fill_fun_defs_tbl t;

(******************************************************************************)
(*                                Pre-processing                              *)
(******************************************************************************)

  (* constify arguments *)
  let cstfbl = Apac.identify_constifiable_functions [] in
  !! Apac.constify_functions_arguments cstfbl [nbAny; cFunDef ""];

(******************************************************************************)
(*                                Processing                                  *)
(******************************************************************************)
  !! Apac.mark_taskable_function "taskable" [nbAny; cFunDef ""];

  (* unfold call taskable *)
  Apac.unfold_funcalls [nbAny; cMark "taskable"];

  (* heapify *)
  (* uses add/remove mark to target sequence with depth > 0 *)
  !! Marks.add "heapify" [nbAny; cMark "taskable"; dBody; cInDepth; cSeq()];
  !! Marks.remove "heapify" [nbAny; cMark "taskable"; dBody; cStrictNew; cSeq()];
  !! Apac.heapify_nested_seq [nbAny; cMark "heapify";];
  !! Marks.remove "heapify" [nbAny; cMark "heapify"];

  (* adds tasks*)
  let fad = Apac.get_functions_args_deps [] in
  !! Apac.insert_tasks_naive fad [nbAny; cMark "taskable"];


  (* adds taskgroup + transforms return to goto *)
  !! Apac.parallel_task_group [nbAny; cMark "taskable"];

  !! Marks.remove "taskable" [nbAny; cMark "taskable"];
)