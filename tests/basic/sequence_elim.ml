open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
    !! Sequence.elim [tIndex ~nb:3 0; cTopFun "main"; cStrict; cSeq()];
    !! Sequence.elim [cSeq ~args:[cVarDef "u"] ()];
    !! Sequence.elim [cSeq ~args_pred:(Target.target_list_one_st (cVarDef "z")) ()];
)
