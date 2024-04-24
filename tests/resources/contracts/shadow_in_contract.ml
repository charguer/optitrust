open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->
  !! Variable.rename ~into:"y" [cVarDef "x"];
)
