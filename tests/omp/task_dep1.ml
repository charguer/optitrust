open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->
  !! Omp.task ~clause:[Shared ["x"]; Depend [Out [Dep_var "x"]]] [sInstr "x = 2"];
  !! Omp.task ~clause:[Shared ["x"]; Depend [In [Dep_var "x"]]] [cFun "printf"];
     let tg = [cSeq ~args_pred:(target_list_one_st [cWriteVar "x"]) ()] in
  !! Omp.single tg;
  !! Omp.parallel tg;
)
