open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.declare_target [] [tBefore; cFunDef "init"];
  !! Omp.end_declare_target [tAfter; cFunDef "init"];
  !! Omp.target [Device "dev"; Map_c (No_map, ["v1";"v2"])] [tAfter; cVarDef "v1"];
  !! Omp.task [Shared ["v1";"v2"]; Depend (Out ["v1";"v2"])] [tAfter; cVarDef "v1"];
  !! Omp.target [Device "dev"; Map_c (To, ["v1";"v2"]);Map_c (From, ["p[0:N]"])] [tAfter; cFun "foo"];
  !! Omp.task [Shared ["v1";"v2";"p"]; Depend (In ["v1";"v2"])] [tAfter; cFun "foo"];
  !! Omp.parallel_for [] [tBefore; cFor "i"];
  !! Omp.taskwait [tAfter; cSeq ~args_pred:(Target.target_list_one_st [cFor "i"]) ()];
)
