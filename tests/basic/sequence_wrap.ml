open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Sequence.wrap [cVarDef "x"];
    !! Sequence.wrap [cSeq ~args_pred:(Target.target_list_one_st (cVarDef "y")) (); cVarDef "y"];
)
