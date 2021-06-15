open Optitrust
open Target

let _ =
  Run.script_cpp (fun _ ->
    (* TODO: Does not work correctly *)
    
    show [cMulti; cSeq ~args_pred:(Target.target_list_one_st (cVarDef "a") )()];
    (* show [cMulti; cSeq ~args_pred:(Target.target_list_one_st (cVarDef "z")) ()]; *)
    Sequence.inline [cMulti; cSeq ~args_pred:(Target.target_list_one_st (cVarDef "y")) ()];
    
    show [cSeq ~args:[cVarDef "x"] ()];
    Sequence.inline [cSeq ~args:[cVarDef "z"] ()];
    (* TODO: try this one too:*)

  )