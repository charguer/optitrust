open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

    show [cVarDef "a";dBody; dArg 0];
)
