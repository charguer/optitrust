open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.single [CopyPrivate ["tmp"]] [tAfter;cVarDef"return_val";];
  !! Omp.master [tBefore; cSeq ~args_pred:(Target.target_list_one_st [cFun "scanf"]) ()];
  !! Omp.barrier [tBefore; sInstr "return_val ="];
  !! Omp.barrier [tAfter; sInstr "return_val ="];
  !! Omp.single [Nowait] [tBefore; cSeq ~args_pred:(Target.target_list_one_st [cFun "free"]) ()];

)