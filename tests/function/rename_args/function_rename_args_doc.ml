open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! Function_basic.rename_args ["x1"] [cFunDef "f"];

)
