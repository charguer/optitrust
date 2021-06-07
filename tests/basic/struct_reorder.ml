
open Optitrust
open Run

(* WORKS *)
let _ = run_unit_test (fun _ ->
        Struct.reorder ~move_before:"x" ["m";"z"] [cTypDef "obj"];
        Struct.reorder ~move_after:"x" ["m"] [cTypDef "obj"];      
)
