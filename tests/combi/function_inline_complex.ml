open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Function.inline_call ~name_result:"r" ~label:"body1" ~inner_fresh_names:["a";"";"b";""] [cFun "g"];
    !! Trace.alternative (fun () ->
      !! Function_basic.bind_intro ~fresh_name:"r" [cFun "g"];
      !! Function.bind_args ["a";"";"b";""] [cFun "g"];
      !! Function_basic.inline_call ~label:"body" [cFun "g"];
      !! Function.elim_body [cLabel "body"];
      !! List.iter (fun x -> Variable_basic.inline ~delete:true [cVarDef x]) (List.filter (fun x -> x <> "") ["a";"";"b";""]);
      !! Variable_basic.init_attach [cVarDef "r"];
      !! Variable_basic.inline ~delete:true [cVarDef "r"];
      !!());
)