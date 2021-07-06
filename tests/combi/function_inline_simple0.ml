open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.inline_call [cFun "g"];
  !! Function.elim_body [Target.cLabel "body"];
  !! Generic.var_init_attach [Target.cVarDef "res"];
  !! Variable.inline ~delete_decl:true [Target.cVarDef "y1"];
  (* !! Function.inline ~name_result:"r" ~label:"body" [cFun "g"]; *)
)