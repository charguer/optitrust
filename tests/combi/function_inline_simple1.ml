open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  (* !! Function.inline ~name_result:"r" [cFun "g"]; *)
  (* TODO: After adding rewrite rules we need to remove if else branches from the inlined function *)
  !! Function.inline_call [cFun "g"];
  !! Function.elim_body (fun s -> s ^ "1") [cLabel "body"];
)