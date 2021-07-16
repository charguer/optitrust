open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.inline_call [cFun "g"];
  !! Function.elim_body (fun s -> s ^ "1") [cLabel "body"];
)