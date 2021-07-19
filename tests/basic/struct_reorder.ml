
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Struct.field_reorder ~move_before:"x" ["m";"z"] [cTypDef "obj"];
    !! Struct.field_reorder ~move_before:"x" ["m"] [cTypDef "obj"];      
)
