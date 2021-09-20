open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
    !! Sequence_basic.elim [tIndex ~nb:3 0; cTopFunDef "main"; cStrict; cSeq()];
    !! Sequence_basic.elim [cSeq ~args:[[cVarDef "u"]] ()];
    !! Sequence_basic.elim [cSeq ~args_pred:(Target.target_list_one_st [cVarDef "z"]) ()];
)
