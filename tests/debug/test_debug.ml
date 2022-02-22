open Optitrust
open Target
open Ast
open Ast_fromto_AstC


let _ = Flags.dump_ast_details := true


let _ = Run.script_cpp (fun _ ->
  let ctx = cFunDef "g" in
  !! Function_basic.bind_intro ~fresh_name:"r" [ctx; cFun "f"];
  !! Function_basic.inline ~body_mark:"body" [ctx;cFun "f"];
  !! Function.elim_body [cMark "body"];
  
  !! Function.inline [cFunDef "g"; cFun "f"];

)
