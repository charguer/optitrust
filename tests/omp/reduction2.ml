open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [Shared ["a";"b";"c";"d";"x";"y";"n"]; Private ["a_p";"b_p";"c_p";"d_p"]] [tAfter; sInstr "d = x[0]"];
  !! Omp.for_ [Private ["i"]] [tBefore; cFor "i"];
  !! Omp.critical "" [tBefore;cSeq ~args_pred:(Target.target_list_one_st [sInstr "a +="]) ()];
)
