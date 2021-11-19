open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ -> 

    !! Function_basic.use_infix_ops ();
)