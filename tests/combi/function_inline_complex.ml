open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Function.inline_call ~name_result:"r" ~inner_fresh_names:["a";"";"b";""] [cFun "g"];
    !! Trace.alternative (fun () ->
      !! Function_basic.bind_intro ~fresh_name:"r" [cFun "g"];
      !! Function.bind_args ["a";"";"b";""] [cFun "g"];
      !! Function_basic.inline_call ~label:"body" [cFun "g"];
      !! Function.elim_body [cLabel "body"];
      (* DONE: skip this ! List.iter (fun x -> Variable_basic.inline ~delete:true [cVarDef x]) (List.filter (fun x -> x <> "") ["a";"";"b";""]);*)
      !! Variable_basic.init_attach [cVarDef "r"];
      (* DONE: this step should only be done if name_result was not provided
         !! Variable_basic.inline ~delete:true [cVarDef "r"]; *)
      !!());

    (* Demo without a result name *)
    (*!! Trace.alternative (fun () ->
        (* DONE: fix this so that it works without the name_result *)
      !! Function.inline_call ~label:"body1" ~inner_fresh_names:["a";"";"b";""] [cFun "g"];
      !!()); *)
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
       !! Function.inline_call ~name_result:"r" (* TODO: remove name_result *) [cFun "g"];
      !!());
)