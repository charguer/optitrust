open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Function_basic.bind_intro ~fresh_name:"r1" [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Function.inline_call [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Function.inline_call [tIndex ~nb:2 0; cFun "vect_add"];
  !! Variable_basic.inline ~delete:true [cVarDef "r1"];
  !! Function_basic.bind_intro ~fresh_name:"r2" [cFun "vect_mul"];
  !! Function.inline_call [cFun "vect_mul"];
  !! Function.inline_call [cFun "vect_add"];
  !! Variable_basic.inline ~delete:true [cVarDef "r2"];

  
  (* !! Function.inline_call ~name_result:"r" ~inner_fresh_names:["v1";"v2"] [tIndex ~nb:2 0; cFun "vect_mul"];   *)
  (* !! Function.inline_call ~name_result:"" ~inner_fresh_names:["v1";"v2"] [tIndex ~nb:2 0; cFun "vect_add"];   *)
  (* Since there are two vect_add function calls the first one need to an explicit bind *)
  (* !! Function.smart_inline ~name_result:"" ~inner_fresh_names:["v1";"v2"] [cVarDef "speed2"; cFun "vect_add"]; *)
  (* !! Function.bind_args  ["v1";"v2"]  [cVarDef "pos2"; cFun "vect_add"]; *)
  (* !! Function.smart_inline ~inner_fresh_names:["v3";"v4"] [cFun "vect_mul"]; *)
  (* !! Function.smart_inline ~name_result:"" ~inner_fresh_names:["v1";"v2"] [cFun "vect_add"]; *)
  (* !! Function.inline ~name_result:"r" ~bind_args:false ~inner_fresh_names:["v1";"v2"] [cVarDef "r"; cFun "vect_add" ~args_pred:(Target.target_list_one_st (cVar "v1"))]; *)
  (* !! Struct.set_explicit [sInstrRegexp ~substr:true "speed2 ="]; *)
  (* !! Function.inline ~name_result:"r" ~bind_args:true ~inner_fresh_names:["v1";"v2"] [cFun "vect_mul"]; *)
  (* !! Function.inline ~name_result:"r" ~bind_args:true ~inner_fresh_names:["v1";"v2"] [cFun "vect_add"]; *)
  (* TODO: Fix the issue with initialization list inside initialization list *)
  (* !! Struct.set_explicit [sInstr "pos2 ="]; *)

  (* !! Function.inline ~name_result:"" ~bind_args:true ~inner_fresh_names:["b2";""] [cTopFun "main"; cFun "bag_push"]; *)

  (* !! Struct.set_explicit [sInstr " = p2"]; *)

  (* !! Struct.set_explicit [sInstr " = p2.pos"]; *)

  (* !! Struct.set_explicit [sInstr " = p2.speed"]; *)

)


