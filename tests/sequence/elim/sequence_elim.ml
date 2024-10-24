open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp ( fun _ ->

  !! Sequence.elim [cSeq ~instrs:[[cVarDef "u"]] ()];
  !! Sequence.elim [cSeq ~instrs_pred:(Target.target_list_one_st [cVarDef "z"]) ()];

  !! Sequence.elim [nbExact 2; cSeq ~instrs:[[]] ()];
  !! Sequence.elim [cMul (); cSeq ()];

)
