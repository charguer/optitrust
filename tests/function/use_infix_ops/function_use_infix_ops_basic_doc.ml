open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Function_basic.use_infix_ops_at [cWriteVar "x"];

)
