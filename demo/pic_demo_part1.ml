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
  !! Struct.set_explicit [cVarDef "speed2"];
  !! Struct.set_explicit [cVarDef "pos2"];
  !! Function.bind_args ["b2";""] [cTopFun "main"; cFun "bag_push"];
  !! Function.inline_call [cTopFun "main"; cFun "bag_push"];
  !! Struct_basic.set_explicit [sInstr " = p2"];
  !! Struct.set_explicit [sInstr " = p2.pos"];
  !! Struct.set_explicit [sInstr " = p2.speed"];

  (* !! Function.inline ~name_result:"" ~bind_args:true ~inner_fresh_names:["b2";""] [cTopFun "main"; cFun "bag_push"]; *)

  (* !! Struct.set_explicit [sInstr " = p2"]; *)

  (* !! Struct.set_explicit [sInstr " = p2.pos"]; *)

  (* !! Struct.set_explicit [sInstr " = p2.speed"]; *)

)


