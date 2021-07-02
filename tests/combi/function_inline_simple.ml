open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    !! Function.bind_intro ~fresh_name:"b" [cFun "g"];
    !!show [cFun "g"];

)