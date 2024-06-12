open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.insert ~reparse:true (stmt "a++;") [tBefore; cVarDef "c"];

)
