open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  (* show [cVarDef "speed2"; cFun "vect_mul"]; *)
  !! Function.inline ~name_result:"r" ["v1";"v2"] [cFun "vect_add"];
  !! Function.bind "r" ["v1";"v2"] [cVarDef "speed2"; cFun "vect_add"];
  !! Function.inline_call ~label:"body" [cVarDef "r"; cFun "vect_add"];
  !! Function.elim_body (fun s -> s ^ "1") [cLabel "body"];
  !! Generic.var_init_attach [cVarDef "r"];
  !! Variable.inline ~delete_decl:true [cVarDef "r"];
  !! Variable.inline ~delete_decl:true [cVarDef "v1"];
  !!! Variable.inline ~delete_decl:true [cVarDef "v2"];
  !! Struct.set_explicit [sInstrRegexp ~substr:true "speed2 ="];
  


  !! Function.bind "r" ["v1";"v2"] [cVarDef "pos2";cFun "vect_mul"];
  !! Function.inline_call ~label:"body" [cVarDef "r"; cFun "vect_mul"];
  !! Function.elim_body (fun s -> s ^ "1") [cLabel "body"];
  !! Generic.var_init_attach [cVarDef "r"];
  !! Variable.inline ~delete_decl:true [cVarDef "r"];
  !! Variable.inline ~delete_decl:true [cVarDef "v1"];
  !! Variable.inline ~delete_decl:true [cVarDef "v2"];

  !! Function.bind "r" ["v1";"v2"] [cVarDef "pos2"; cFun "vect_add"];
  !! Function.inline_call ~label:"body" [cVarDef "r"; cFun "vect_add"];
  !! Function.elim_body (fun s -> s ^ "1") [cLabel "body"];
  !! Generic.var_init_attach [cVarDef "r"];
  !! Variable.inline ~delete_decl:true [cVarDef "r"];
  !! Variable.inline ~delete_decl:true [cVarDef "v1"];
  !! Variable.inline ~delete_decl:true [cVarDef "v2"];

  !! Struct.set_explicit [sInstrRegexp ~substr:true "pos2 ="];
  !!! Function.bind_args ["b2";""] [cTopFun "main"; cFun "bag_push"];
  
  !! Function.inline_call ~label:"body" [cTopFun "main"; cFun "bag_push"];
  !! Function.elim_body (fun s -> s ^ "1") [cLabel "body"];
  !! Struct.set_explicit [sInstr " = p2"];
  !! Struct.set_explicit [sInstr " = p2.pos"];
  !! Struct.set_explicit [sInstr " = p2.speed"];
  
  

  (* !!! Struct.set_explicit [sInstrRegexp ~substr:true "speed2 ="];

  !! Function.bind "r" ["v3";"v4"] [cFun "vect_mul"];
  !! Function.inline_call ~label:"body" [cFun "vect_mul"];
  !! Function.elim_body (fun s -> s ^ "1") [cLabel "body"];
  !! Generic.var_init_attach [cVarDef "r"];
  !! Variable.inline ~delete_decl:true [cVarDef "r"];

  !! Function.bind "r" ["v5";"v6"] [cFun "vect_add"];
  !! Function.inline_call ~label:"body" [cVarDef "r"; cFun "vect_add"];
  !! Function.elim_body (fun s -> s ^ "1") [cLabel "body"];
  !! Generic.var_init_attach [cVarDef "r"];
  !! Variable.inline ~delete_decl:true [cVarDef "r"];
  
  !! Struct.set_explicit [cVarDef "speed2"];



  !! Function.bind "r" [""; "a"] [cVarDef "pos2"; cFun "vect_mul"];
  !! Function.inline ~name_result:"r" ~label:"body" [cFun ""]; *)
  (* After adding function inline transformations *)



)


