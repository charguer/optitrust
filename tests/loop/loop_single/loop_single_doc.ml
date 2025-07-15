open Optitrust
open Prelude

let _ = Run.script_cpp (fun () -> !!Loop.loop_single [ cVarDef "k" ])
