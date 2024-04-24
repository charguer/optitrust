open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

    !! Function.use_infix_ops ~indepth:true [cFunDef "main"; dBody];
)
