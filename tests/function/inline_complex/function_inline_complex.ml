open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

    !! Function.inline ~args:["a";"";"b";""] [cCall "g"];

    !! Trace.restore_original();
    !! Function.bind_args ["a";"";"b";""] [cCall "g"];
    !! Function_basic.inline ~body_mark:"body" [cCall "g"];
    !! Function.elim_body ~resname:"r" [cMark "body"];

    (* Demo without a result name *)
    !! Trace.restore_original();
    !! Function.inline ~args:["a";"";"b";""] [cCall "g"];

    !! Trace.restore_original();
    let r = "res_temp" in
    !! Function.bind_args ["a";"";"b";""] [cCall "g"];
    !! Function_basic.inline ~body_mark:"body" [cCall "g"];
    !! Function.elim_body ~resname:r [cMark "body"];
    !! Variable.inline [cVarDef r];

    (* Demo without naming the arguments, observe the duplicated call to h *)
    !! Trace.restore_original();
    !! Function.inline ~resname:"r" [cCall "g"];

    !! Trace.restore_original();
    !! Function.inline [cCall "g"];
)
