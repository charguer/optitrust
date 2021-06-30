open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Loop.fusion [cSeq ~args_pred:(Target.targdepth_decret_list_one_st (cFor "i")) ()];
)
