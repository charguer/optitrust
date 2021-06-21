open Optitrust
open Target


let _ = Run.script_cpp ( fun _ -> 
     Struct.set_implicit [cSeq ~args_pred:(Target.target_list_one_st (cInstr "b.x = p.x")) ()];
    )
