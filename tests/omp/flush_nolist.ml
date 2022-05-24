open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->
  !! Omp.flush [] [tAfter; cFunDef "f1"; sInstr "= 1"];
  !! Omp.barrier [tBefore; cFunDef "f2"; sInstr "= 2"];
  !! Omp.barrier [tAfter; cFunDef "f2"; sInstr "= 2"];
  !! Omp.parallel [Reduction (Plus, ["sum"]); Num_threads "10"] [tBefore; cSeq ~args_pred:(Target.target_list_one_st [sInstr "sum += j"]) ()];

)
