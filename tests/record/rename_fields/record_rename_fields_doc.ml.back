open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    !! Record_basic.rename_fields (fun f -> String.uppercase_ascii f) [cTypDef "obj"];
)
