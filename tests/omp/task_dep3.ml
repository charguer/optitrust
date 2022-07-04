open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.task ~clause:[Shared ["x"]; Depend [Out [Dep_var "x"]]] [nbMulti; cWriteVar "x"];
  !! Omp.taskwait [cFun "printf"];
    let tg = [cSeq ~args_pred:(target_list_one_st [cWriteVar "x"]) ()] in
  !! Omp.single tg;
  !! Omp.parallel tg;
)
