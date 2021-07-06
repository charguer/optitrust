open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.inline_call [cFun "g"];
  !! Function.elim_body (fun s -> s ^ "1") [cLabel "body"];
  (* !! Generic.var_init_attach [Target.cVarDef "res"]; *) (* More then one occurence of the variable *)
)