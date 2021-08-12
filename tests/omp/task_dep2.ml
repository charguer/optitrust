open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.task [Shared_c ["x"]; Depend (In ["x"])] [tFirst;cSeq ~args_pred:(Target.target_list_one_st (sInstr "x = 2"))()];
  !! Omp.task [Shared_c ["x"]; Depend (Out ["x"])] [tBefore; sInstr "x = 2"];
  !! Omp.single []  [tAfter; cVarDef "x"];
  !! Omp.parallel []  [tAfter; cVarDef "x"];
)