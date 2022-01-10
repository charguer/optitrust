open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.threadprivate ["x"] [tBefore; cVarDef "x"];
  !! Omp.parallel [Default None_; Private ["a"]; Shared ["x"; "c"]] [tBefore; cSeq ~args_pred:(Target.target_list_one_st [cVarDef "j"]) ()];
  !! Omp.for_ [FirstPrivate ["y"]] [tAfter; occIndex ~nb:3 0; sInstr "z[i] ="];  
)
