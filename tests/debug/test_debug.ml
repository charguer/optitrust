open Optitrust
open Target
open Path


let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

   show [cFunDef "f"];
)
