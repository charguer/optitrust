open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
    !! Sequence.inline [tIndex ~nb:3 0; cTopFun "main"; cStrict; cSeq()];
    !! Sequence.inline [cSeq ~args:[cVarDef "u"] ()];
    !! Sequence.inline [cSeq ~args_pred:(Target.target_list_one_st (cVarDef "z")) ()];
)