open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.bind ~fresh_name:"r" ~args:["a";"";"b";""] [cCall "g"];

  (* default is to not name any of the arguments *)
  !! Trace.restore_original();
  !! Function.bind ~fresh_name:"r" [cCall "g"];

  (* default name is "res" for the result *)
  !! Trace.restore_original();
  !! Function.bind [cCall "g"];

)
