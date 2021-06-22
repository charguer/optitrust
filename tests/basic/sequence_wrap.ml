open Optitrust
open Target

(* Transformation works but there is one issue with the path *)

let _ =
  Run.script_cpp (fun _ ->

    Sequence.wrap [cVarDef "x"];
    Sequence.wrap [cSeq ~args_pred:(Target.target_list_one_st (cVarDef "y")) (); cVarDef "y"];
   

  )