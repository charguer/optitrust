open Optitrust
open Target

(* Works *)
let _ =
  Run.script_cpp (fun _ ->
    
    Sequence.insert [cBefore; cVarDef "x"] ["int a = 5";"const float b = 5.0"];

    (* Sequence.insert [cSeq ~args_pred:(Target.target_list_one_st (cVarDef "z")) ()]; *)

  )