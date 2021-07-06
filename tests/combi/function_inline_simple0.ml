open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.inline_call [cFun "g"];
  !! Function.elim_body (fun s -> s ^ "1") [cLabel "body"];
  !! Generic.var_init_attach [cVarDef "res"];
  !! Variable.inline ~delete_decl:true [cVarDef "y1"];
  (* !! Function.inline ~name_result:"r" ~label:"body" [cFun "g"]; *)
)