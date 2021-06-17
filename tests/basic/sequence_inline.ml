open Optitrust
open Target
(* Works *)
let _ =
  Run.script_cpp (fun _ ->
    
    !! Sequence.inline [cSeq ~args:[cVarDef "y"] ()];

    (* show [cMulti; cSeq ~args_pred:(Target.target_list_one_st (cVarDef "z")) ()]; *)
    (* Sequence.inline [cMulti; cSeq ~args_pred:(Target.target_list_one_st (cVarDef "y")) ()]; *)
    
    (* show [cSeq ~args:[cVarDef "x"] ()]; *)
    (* TODO: try this one too:*)

  )