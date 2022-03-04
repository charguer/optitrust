open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp ~parser:Parsers.Clang (fun _ ->
    
    !! Function.inline ~resname:"r" [cFun "g"; cFun "f"];
    !!! Function.inline ~resname:"r" [cVarDef "p"; cFun "f"];
)
