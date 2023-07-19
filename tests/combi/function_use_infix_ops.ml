open Optitrust
open Target
open Syntax


let _ = Run.script_cpp (fun _ ->

    !! Function.use_infix_ops ~indepth:true [cFunDef "main"; dBody];
)