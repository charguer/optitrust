open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

   
   !! Function.bind_intro ~fresh_name:"c" [cFun "f"];
   !! Function.inline [cFun "f"];
   !! Variable.inline [cVarDef "c"];
   
   (* !! Function.inline [cFun "f"]; *)
   
)
 