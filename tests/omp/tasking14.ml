open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  
  !! Omp.task [nbMulti; cFun "bar"];
  !! Omp.task [nbMulti; cFor_c ""];
  !! Omp.task ~clause:[If "0"] [occIndex 0;cSeq ~args_pred:(target_list_one_st [cFor_c ""]) ()];
  !! Omp.task ~clause:[Final "1"] [occIndex 1;cSeq ~args_pred:(target_list_one_st [cFor_c ""]) ()];
  
)
