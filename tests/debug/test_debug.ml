open Optitrust
open Target
open Path

let _ = Flags.dump_ast_details := true


let _ = Run.script_cpp  ~parser:(Parsers.Menhir) (fun _ ->

   (* !! Function.inline [cVarDef "x"; cFun "test"]; *)
   
   !! Function.inline [nbMulti; cFor_c"i"; cFun "test"];
   
)
 