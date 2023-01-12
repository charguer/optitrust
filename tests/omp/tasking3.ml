open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 2 [cVarDef "p"];
  !! Sequence_basic.intro 1 [cSeq ~args_pred:(Target.target_list_one_st [cVarDef "p"]) ()];
  !! Omp.task [cFun "process"];
     let tg_p = cSeq ~args_pred:(Target.target_list_one_st [cVarDef "p"]) () in 
  !! Omp.single [tg_p];
  !! Omp_basic.parallel [cSeq ~args:[[tg_p]] ()];

)

