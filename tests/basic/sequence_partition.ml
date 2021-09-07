open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  (* ARTHUR: why is this giving 2 results? 
     show [nbMulti; cFunDef "main"; cSeq(); cSeq()];  
     ARTHUR: maybe "show" should force nbMulti always
   *)

  let tg = [cSeq ~args_pred:(Target.target_list_one_st (cVarDef "a")) ()] in (* LATER: simplify *)
  (* Error message if invalid partition:
     !! Sequence_basic.partition ~visible:true [2;3] tg; *)

  !! Trace.alternative (fun () ->
     Sequence_basic.partition ~visible:true [] tg;
     !!());
  
)