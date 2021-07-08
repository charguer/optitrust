open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    
    
    !! Function.inline ~name_result:"r" ~bind_args:true ["a";"";"b";""] [cFun "g"];
    
)