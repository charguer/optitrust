open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !!Function.bind ~fresh_name:"r" ~inner_fresh_names:["a";"";"b";""] [cFun "g"];
)
