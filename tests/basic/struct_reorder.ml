
open Optitrust
open Target

(* WORKS *)
let _ = Run.script_cpp (fun _ ->
        Struct.reorder ~move_before:"x" ["m";"z"] [cTypDef "obj"];
        Struct.reorder ~move_after:"x" ["m"] [cTypDef "obj"];      
)
