open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

    !! Function.inline ~resname:"r" ~args:["a";"";"b";""] [cFun "g"];

    !! Trace.restore_original();
    !! Function_basic.bind_intro ~fresh_name:"r" [cFun "g"];
    !! Function.bind_args ["a";"";"b";""] [cFun "g"];
    !! Function_basic.inline ~body_mark:"body" [cFun "g"];
    !! Function.elim_body [cMark "body"];
    !! Variable_basic.init_attach [cVarDef "r"];

    (* Demo without a result name *)
    !! Trace.restore_original();
    !! Function.inline ~args:["a";"";"b";""] [cFun "g"];

    !! Trace.restore_original();
    let r = "res_temp" in
    !! Function_basic.bind_intro ~fresh_name:r ~const:false [cFun "g"];
    !! Function.bind_args ["a";"";"b";""] [cFun "g"];
    !! Function_basic.inline ~body_mark:"body" [cFun "g"];
    !! Function.elim_body [cMark "body"];
    !! Variable_basic.init_attach [cVarDef r];
    !! Variable.inline [cVarDef r];

    (* Demo without naming the arguments, observe the duplicated call to h *)
    !! Trace.restore_original();
    !! Function.inline ~resname:"r" [cFun "g"];

    !! Trace.restore_original();
    !! Function.inline [cFun "g"];
)
