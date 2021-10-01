open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  (* TODO: ARTHUR why is this giving 2 results? 
     show [nbMulti; cFunDef "main"; cSeq(); cSeq()];  
     ARTHUR: maybe "show" should force nbMulti always
   *)  
  let tg = [cSeq ~args_pred:(Target.target_list_one_st [cVarDef "a"]) ()] in (* LATER: simplify *)
  !! Sequence_basic.partition ~visible:true [2;3;2] tg;
  

  (* Error message if the sum of blocks does not correspond to the number trms in the sequence*)
  !! Tools.failure_expected (fun _ -> 
    Sequence_basic.partition ~visible:true [2;3] tg;);

  !! Trace.alternative (fun () ->
     Sequence_basic.partition ~visible:true [1;2;1;3] tg;
     !!()); 
  
)