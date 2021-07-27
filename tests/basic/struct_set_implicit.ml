open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->
  let tg = [cSeq ~args_pred:(Target.target_list_one_st (sInstr ~substr:false "b.x = p.x")) ()] in
  !! Struct_basic.set_implicit tg;
)

