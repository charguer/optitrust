open Optitrust
open Target

(* Works *)
let _ =
  Run.script_cpp (fun _ ->
    
    Sequence.inline [cSeq ~args:[cVarDef "u"] ()];

    !!Sequence.inline [cSeq ~args_pred:(Target.target_list_one_st (cVarDef "z")) ()];

  )