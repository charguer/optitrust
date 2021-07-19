open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  show [nbMulti;cSeq ~args:[cFor "i"]()];
  !! Loop.fusion [cSeq ~args_pred:(target_list_one_st (cFor "i")) ()];
)