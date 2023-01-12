open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Sequence_basic.intro 1 [cFor_c ""];
     let tg_seq_for = cSeq ~args:[[cFor_c ""]] () in 
  !! Sequence_basic.intro 2 [cVarDef "i"];
     let tg_seq_i = cSeq ~args_pred:(target_list_one_st [cVarDef "i"]) () in 
  !! Sequence_basic.intro 1 [tg_seq_i];
  
  !! Omp.task [cFun "process"];
  !! Omp.task ~clause:[Untied] [tg_seq_for];
  !! Omp.single [tg_seq_i];
  !! Omp_basic.parallel [cSeq ~args:[[tg_seq_i]] ()];
)
