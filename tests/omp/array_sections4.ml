open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.target_data [Map_c (No_map,["A[0:10]"])] [tAfter; cVarDef "A"];
  !! Omp.target [Map_c (No_map, ["p[3:7]"])] [tBefore; cSeq ~args_pred:(Target.target_list_one_st (sInstr "A[2] = 0")) ()];
)
 