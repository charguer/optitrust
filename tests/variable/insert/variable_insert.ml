open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Variable.insert ~name:"a" ~value:(lit "300") [ tAfter; cTypDef "vect"];
  !! Variable.insert ~name:"b" ~value:(lit "500") [ tAfter; cTypDef "vect"];

)
