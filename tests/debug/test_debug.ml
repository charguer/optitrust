open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp ~parser:Parsers.Clang (fun _ ->
   
  show ~types:true [cWriteVar "y";dLHS];
)
 