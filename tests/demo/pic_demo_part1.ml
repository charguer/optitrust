open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  (* Since there are two vect_add function calls the first one need to an explicit bind *)
  (* !! Function.smart_inline ~name_result:"r" ~bind_args:true ~inner_fresh_names:["v1";"v2"] [cVarDef "speed2"; cFun "vect_add"]; *)

  !! Function.bind "r" ["v1";"v2"]  [cVarDef "speed2"; cFun "vect_add"];
  show [nbMulti; cVarDef "r"; cFun "vect_add" ~args_pred:(Target.target_list_one_st (cVar "v1"))];
  !! Function.inline ~name_result:"r" ~bind_args:false ~inner_fresh_names:["v1";"v2"] [cVarDef "r"; cFun "vect_add" ~args_pred:(Target.target_list_one_st (cVar "v1"))];

  !! Struct.set_explicit [sInstrRegexp ~substr:true "speed2 ="];

  !! Function.inline ~name_result:"r" ~bind_args:true ~inner_fresh_names:["v1";"v2"] [cFun "vect_mul"];

  !! Function.inline ~name_result:"r" ~bind_args:true ~inner_fresh_names:["v1";"v2"] [cFun "vect_add"];

  (* TODO: Fix the issue with initialization list inside initialization list *)
  !! Struct.set_explicit [sInstr "pos2 ="];

  !! Function.inline ~name_result:"" ~bind_args:true ~inner_fresh_names:["b2";""] [cTopFun "main"; cFun "bag_push"];

  !! Struct.set_explicit [sInstr " = p2"];

  !! Struct.set_explicit [sInstr " = p2.pos"];

  !! Struct.set_explicit [sInstr " = p2.speed"];

)


