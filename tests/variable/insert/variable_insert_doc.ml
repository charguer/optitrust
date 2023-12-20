open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Variable.insert ~typ:(ty "int") ~name:"b" ~value:(lit "2") [tAfter; cVarDef "a"];

)
