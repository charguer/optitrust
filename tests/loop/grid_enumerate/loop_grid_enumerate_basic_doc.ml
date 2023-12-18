open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.grid_enumerate [("x", lit "3"); ("y", lit "4")] [cFor "c"];

)
