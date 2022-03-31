open Optitrust
open Target
open Path

let _ = Run.script_cpp  ~parser:(Parsers.Menhir) (fun _ ->

   (* !! Function.inline [cVarDef "x"; cFun "test"]; *)
   
   !! Function.inline [nbMulti; cFun "test"];
   
)
 