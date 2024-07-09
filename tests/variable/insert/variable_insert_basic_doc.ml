open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.insert ~reparse:true ~name:"b" ~typ:(ty "int") ~value:(lit "2") [tAfter; cVarDef "a"];
)
