open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Function_basic.bind_intro ~fresh_name:"a" [cFun "g"];

)
