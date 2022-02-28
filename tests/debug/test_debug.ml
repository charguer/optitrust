open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Function.inline [cFun "f"];
  !! Trace.alternative (fun () ->
      !! Function_basic.bind_intro ~fresh_name:"r" ~const:false [cFun "f"];
      !! Function_basic.inline ~body_mark:"body" [cFun "f"];
      !! Function.elim_body [cMark "body"];
      !! Variable_basic.init_attach [cVarDef "r"];
      !! Variable.inline [cVarDef "r"];
      !!());

)

