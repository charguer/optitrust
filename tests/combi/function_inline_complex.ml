open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Function.inline_call ~name_result:"r" ~args:["a";"";"b";""] [cFun "g"];
    !! Trace.alternative (fun () ->
      !! Function_basic.bind_intro ~fresh_name:"r" [cFun "g"];
      !! Function.bind_args ["a";"";"b";""] [cFun "g"];
      !! Function_basic.inline_call ~label:"body" [cFun "g"];
      !! Function.elim_body [cLabel "body"];
      !! Variable_basic.init_attach [cVarDef "r"];
      !!());

    (* Demo without a result name *)
    !! Trace.alternative (fun () ->
    (* DONE: rename arguments
    !! Function.inline_call ~label:"body1" ~args:["a";"";"b";""] ~vars:(Suffix "_1")  [cFun "g"]; *)
      !! Function.inline_call ~label:"body1" ~args:["a";"";"b";""] [cFun "g"];
      !!());
    !! Trace.alternative (fun () ->
      let r = "res_temp" in
      !! Function_basic.bind_intro ~fresh_name:r [cFun "g"];
      !! Function.bind_args ["a";"";"b";""] [cFun "g"];
      !! Function_basic.inline_call ~label:"body" [cFun "g"];
      !! Function.elim_body [cLabel "body"];
      !! Variable_basic.init_attach [cVarDef r];
      !! Variable_basic.inline ~delete:true [cVarDef r];
      !!());

    (* Demo without naming the arguments, observe the duplicated call to h *)
    !! Trace.alternative (fun () ->
       !! Function.inline_call ~name_result:"r" [cFun "g"];
       !!());
    !! Trace.alternative (fun () ->
       !! Function.inline_call [cFun "g"];
       !!());
)