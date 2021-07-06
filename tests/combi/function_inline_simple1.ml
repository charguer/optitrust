open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.inline_call [cFun "g"];
  !! Function.elim_body (fun s -> s ^ "1") [cLabel "body"];

  (* !! Generic.var_init_attach [Target.cVarDef "res"]; *) (* Doesn't work because the algorithm can't see the assigment in depth*)
  (* !! Function.inline ~name_result:"r" ~label:"body" [cFun "g"]; *)
)