open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target_data [Map_c (No_map,["A[0:4]"])] [tAfter; cVarDef "A"];
  !! Omp.target [Map_c (No_map, ["p[3:20]"])] [tBefore; cSeq ~args_pred:(Target.target_list_one_st (sInstr "A[2] = 0")) ()];
)
 