open Optitrust
open Target


let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->
  
  (* inlining a function with if else branches *)
  !! Function.inline [cTopFunDef "main"; cFun "g"];
  !! Trace.alternative (fun () ->
    !! Marks.add "__inline_instruction" [Target.cVarDef "z"];
    !! Function_basic.bind_intro ~my_mark:"__inline_" ~fresh_name:"__TEMP_Optitrust" ~const:false [cTopFunDef "main"; cFun "g"];
    !! Function_basic.inline ~body_mark:"__TEMP_BODY" [cMark "__inline_"];
    !! Function.elim_body [cMark "__TEMP_BODY"];
    !! Variable_basic.init_attach [cMark "__inline_"];
    !! Variable.inline [cMark "__inline_"];
    !! Variable.inline_and_rename [cMark "__inline_instruction"];
    
    !!());
)
