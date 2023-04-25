open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  (* Note: we use here 4 instead of "NT" as in the example because macros are not supported in OptiTrust.  *)
  !! Omp_basic.set_num_threads 4 [tAfter; cVarDef "section_count"]; 
  
  !! Omp_basic.set_dynamic 0 [tAfter; cVarDef "section_count"];
  
  let tg_inn_seq = cSeq ~args_pred:(target_list_one_st [cFun "printf"])() in 
  !! Omp_basic.section [nbMulti; tg_inn_seq];
  let tg_seq = cSeq ~args_pred:(target_list_one_st [tg_inn_seq]) () in 
  !! Omp_basic.parallel_sections ~clause:[FirstPrivate ["section_count"]] [tg_seq];
  !! Omp_basic.parallel [tg_seq];

)
