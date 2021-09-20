open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Omp.task [Final "pos > LIMIT"; Mergeable] [tBefore; tIndex ~nb:3 0; cIf ()];
  !! Omp.task [Final "pos > LIMIT";Mergeable] [tAfter; tIndex ~nb:2 0; cSeq ~args_pred:(Target.target_list_one_st [cVarDef "new_state"]) ()];
  !! Omp.taskwait [tAfter; tIndex ~nb:2 1; cSeq ~args_pred:(Target.target_list_one_st [cVarDef "new_state"]) ()];
)
