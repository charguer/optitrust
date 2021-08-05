open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.partition ~visible:true [2;3;2] [cSeq ~args_pred:(Target.target_list_one_st (cVarDef "a")) ()];
)