open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  let my_mark = "__my_mark" in
  !! Function_basic.bind_intro ~my_mark ~fresh_name:"r" [cFun "g"];
  !! Function_basic.bind_intro ~fresh_name:"s" [cFun "h"];
)
