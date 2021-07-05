open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.inline_call [cFun "g"];
  !! Function.elim_body [Target.cLabel "r"];
  !! Generic.var_init_attach [Target.cVarDef "r"];
  !! Variable.inline [Target.cVarDef "r"];
  !! Function.inline ~name_result:"r" ~label:"body" [cFun "g"];
)