open Optitrust
open Target


(* TODO: Add support for union parsing without typedef *)
let _ = Run.script_cpp (fun _ ->
  
  !! Omp.parallel [cSeq ~args_pred:(Target.target_list_one_st (sInstr "u.n++")) ()];
  !! Omp.atomic (Some Update) [tBefore; sInstr "u.n++"];
  !! Omp.atomic (Some Update) [tBefore; sInstr "u.x += 1.0"];
)
