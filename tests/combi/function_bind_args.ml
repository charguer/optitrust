open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.bind_args ["a";"";"b";""] [nbMulti;cFun "g"]; 
)
