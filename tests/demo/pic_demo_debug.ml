open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  (* show [cVarDef "speed2"; cFun "vect_mul"]; *)
  !! Function.inline_call ~name_result:"r" ~label:"body" [cVarDef "speed2";cFun "vect_add"];
  !! Function.elim_body (fun s -> s ^ "1") [cLabel "body"];
  !!! Generic.var_init_attach [cVarDef "r"];
  !! Variable.inline ~delete_decl:true [cVarDef "r"];
  !! Function.bind "r" [""; "a"] [cVarDef "pos2"; cFun "vect_mul"];
  !! Function.inline ~name_result:"r" ~label:"body" [cFun ""];
  (* After adding function inline transformations *)
  


)