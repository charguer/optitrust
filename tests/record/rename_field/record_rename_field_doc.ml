open Optitrust
open Target



let _ = Run.script_cpp (fun _ ->
    !! Record.rename_field "pos" ~into:"POS" [cTypDef "obj"];
  )
