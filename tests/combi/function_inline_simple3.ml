open Optitrust
open Target

(* TODO: This is not translated properly *)
let _ = Run.script_cpp (fun _ ->
  !! Function.inline_call [cFun "h"];
  !! Function.elim_body [Target.cLabel "body"];
)