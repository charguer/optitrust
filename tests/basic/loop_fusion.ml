open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
        !! Loop.fusion [cSeq ~args_pred:(Target.target_list_one_st ( cForSimple "i")) ()];
)
