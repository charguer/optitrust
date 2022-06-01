open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Omp.parallel_sections ~clause:[FirstPrivate ["section_count"]] [cVarDef "section_count"];
  !! Omp.set_num_threads "4" [cVarDef "section_count"];
  !! Omp.set_dynamic 0 [cVarDef "section_count"];
  !! Omp.section [nbMulti; cSeq ~args_pred:(target_list_one_st [cFun "printf"])()];

)
