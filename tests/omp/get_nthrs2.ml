open Optitrust
open Target


let _ = Run.script_cpp (fun _ -> 

  !! Omp_basic.get_thread_num "i" [tBefore; cFun "work"];
  !! Sequence_basic.intro 2 [cWriteVar "i"];
  !! Omp_basic.parallel ~clause:[Private ["i"]] [cSeq ~args_pred:(target_list_one_st [cFun "work"]) ()];

)
