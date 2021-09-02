open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.bind ~fresh_name:"r" ~inner_fresh_names:["a";"";"b";""] [cFun "g"];

  (* default is to not name any of the arguments *)
  !! Trace.alternative (fun () ->
    Function.bind ~fresh_name:"r" [cFun "g"];
    !!();
  );

  (* default name is "res" for the result *)
  !! Trace.alternative (fun () ->
    Function.bind [cFun "g"];
    !!();
  )
)
