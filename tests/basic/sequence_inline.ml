open Optitrust
open Target

(* TODO: rename this file to sequence_inline.ml *)

let _ =
  Run.script_cpp (fun _ ->
    (* TODO: Does not work correctly *)
    Sequence.inline [cSeq ~args:[cVarDef "y"]];
    (* TODO: try this one too:*)
    (* Sequence.inline [cMulti; cSeq ~args_pred:(Target.target_list_one_st (cVarDef "y"))]; *)

  )