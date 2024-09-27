open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

    !! Function.inline ~resname:"r" ~args:["a";"";"b";""] [cCall "g"];

    !! Trace.restore_original();
    !! Function_basic.bind_intro ~fresh_name:"r" [cCall "g"];
    !! Function.bind_args ["a";"";"b";""] [cCall "g"];
    !! Function_basic.inline ~body_mark:"body" [cCall "g"];
    !! Function.elim_body [cMark "body"];
    !! Instr_basic.move ~dest:[tBefore; occFirst; cWriteVar "r"] [cVarDef "r"];
    !! Variable_basic.init_attach [cVarDef "r"];

    (* Demo without a result name *)
    !! Trace.restore_original();
    !! Function.inline ~args:["a";"";"b";""] [cCall "g"];

    !! Trace.restore_original();
    let r = "res_temp" in
    !! Function_basic.bind_intro ~fresh_name:r ~const:false [cCall "g"];
    !! Function.bind_args ["a";"";"b";""] [cCall "g"];
    !! Function_basic.inline ~body_mark:"body" [cCall "g"];
    !! Function.elim_body [cMark "body"];
    !! Instr_basic.move ~dest:[tBefore; occFirst; cWriteVar r] [cVarDef r];
    !! Variable_basic.init_attach [cVarDef r];
    !! Variable.inline [cVarDef r];

    (* Demo without naming the arguments, observe the duplicated call to h *)
    !! Trace.restore_original();
    !! Function.inline ~resname:"r" [cCall "g"];

    !! Trace.restore_original();
    !! Function.inline [cCall "g"];
)
