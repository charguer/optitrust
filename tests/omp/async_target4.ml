open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [Num_threads "2"] [tBefore; cVarDef "p"];
  !! Omp.single [] [tBefore; cSeq ~args_pred:(Target.target_list_one_st [cCall "init"]) ()];
  !! Omp.task [Depend (Out ["v1"])] [tBefore; occIndex ~nb:2 0; cCall "init"];
  !! Omp.task [Depend (Out ["v2"])] [tBefore; occIndex ~nb:2 1; cCall "init"];
  !! Omp.target [Nowait; Depend (In ["v1";"v2"]); Depend (Out ["p"]);Map_c (To, ["v1";"v2"]); Map_c (From, ["p"])] [tBefore; cFor "i"];
  !! Omp.parallel_for [Private ["i"]] [tBefore; cFor "i"];
  !! Omp.task [Depend (In ["p"])] [tBefore; cCall "output"];
)
