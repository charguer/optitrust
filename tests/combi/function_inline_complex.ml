open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    
    !! Function.inline ~resname:"r" ~args:["a";"";"b";""] [cFun "g"];
    !! Trace.alternative (fun () ->
      !! Function_basic.bind_intro ~fresh_name:"r" [cFun "g"];
      !! Function.bind_args ["a";"";"b";""] [cFun "g"];
      !! Function_basic.inline ~body_mark:"body" [cFun "g"];
      !! Function.elim_body [cMark "body"];
      !! Variable_basic.init_attach [cVarDef "r"];
      !!());

    (* Demo without a result name *)
    !! Trace.alternative (fun () ->
      !! Function.inline ~args:["a";"";"b";""] [cFun "g"];
      !!());
    !! Trace.alternative (fun () ->
      let r = "res_temp" in
      !! Function_basic.bind_intro ~fresh_name:r ~const:false [cFun "g"];
      !! Function.bind_args ["a";"";"b";""] [cFun "g"];
      !! Function_basic.inline ~body_mark:"body" [cFun "g"];
      !! Function.elim_body [cMark "body"];
      !! Variable_basic.init_attach [cVarDef r];
      !! Variable.inline [cVarDef r];
      !!());

    (* Demo without naming the arguments, observe the duplicated call to h *)
    !! Trace.alternative (fun () ->
       !! Function.inline ~resname:"r" [cFun "g"];
       !!());
    !! Trace.alternative (fun () ->
       !! Function.inline [cFun "g"];
       !!());
)