open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp ( fun _ ->

  !! Sequence_basic.elim [cSeq ~args:[[cVarDef "u"]] ()];
  !! Sequence_basic.elim [cSeq ~args_pred:(Target.target_list_one_st [cVarDef "z"]) ()];

  (* Shadowing behaviour (1). *)
  !! Loop_basic.unroll [cFor "i"];
  (* TODO: will trigger rename, should this throw an error?
  !! Trace.failure_expected (fun _e -> true) (fun () -> *)
  !! Sequence_basic.elim [nbMulti; cSeq ~args:[[cVarDef "s0"]] ()];

  (* Shadowing behaviour (2). *)
  (* TODO: figure out what should happen here
  !! Trace.failure_expected (fun _e -> true) (fun () ->
  !! Sequence_basic.elim [cSeq ~args:[[cVarDef "s1"]] ()]; *)
)
