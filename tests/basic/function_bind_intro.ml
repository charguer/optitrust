open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function_basic.bind_intro ~fresh_name:"r" [cFun "g"];
)