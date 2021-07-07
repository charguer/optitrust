open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    (* show [cFun "g"]; *)
    (* !! Function.bind_intro ~fresh_name:"r" [cFun "g"]; *)
    (* !! Function.bind_args ["a"; ""; "b"; ""] [cFun "g"];  *)
    !! Function.bind "r" ["a"; ""; "b"; ""] [cFun "g"];
    !! Function.inline ~name_result:"r" ~label:"body" [cFun "g"];
)