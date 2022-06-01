open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

  !! Omp_basic.parallel_for ~clause:[Default Shared_m; Private ["iam"; "nt"; "ipoints"; "istart"]] [cSeq ~args_pred:(Target.target_list_one_st [cFun "subdomain"]) ()];

)
