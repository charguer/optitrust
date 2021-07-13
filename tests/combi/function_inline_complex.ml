open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    
    
    !! Function.inline ~name_result:"r" ~bind_args:true ~inner_fresh_names:["a";"";"b";""] [cFun "g"];
    
)