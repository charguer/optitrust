open Optitrust
open Target

(* Transformation works but there is one issue with the path *)

let _ =
  Run.script_cpp (fun _ ->
    

    show [cSeq ~args:[cVarDef "y"] ()];
    Sequence.inline [cMulti;cSeq ~args:[cVarDef "y"] ()];
    (* TODO: try this one too:*)
    Sequence.inline [cSeq ~args_pred:(Target.target_list_one_st (cVarDef "y")) ()];

  )