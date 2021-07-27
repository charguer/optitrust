open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->
  !! Struct_basic.set_implicit [cSeq ~args_pred:(Target.target_list_one_st (sInstr ~substr:false "b.x = p.x")) ()];
)

