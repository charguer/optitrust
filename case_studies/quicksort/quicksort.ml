open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun () ->
  !! Apac.constify [nbAny; cFunDefAndDecl ""];
  !! Apac.mark_taskification_candidates [nbAny; cFunDef ""];

  (* unfold call taskable
  Apac.unfold_funcalls [
    nbAny; cDiff [[cMark "taskable"]] [[cHasTypeAst (typ_unit ())]]
  ];

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

  !! Marks.remove "taskable" [nbAny; cMark "taskable"];*)
)