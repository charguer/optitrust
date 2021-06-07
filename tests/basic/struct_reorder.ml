
open Optitrust
open Run

let _ = run_unit_test (fun _ ->
        Struct.reorder ~move_before:"x" ["m";"z"] [cTypDef "obj"];
        Struct.reorder ~move_after:"x" ["m"] [cTypDef "obj"];      
)
