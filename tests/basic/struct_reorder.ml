
open Optitrust
open Target

(* Works *)
let _ = Run.script_cpp (fun _ ->
        !!Struct.reorder ~move_before:"x" ["m";"z"] [cTypDef "obj"];
        Struct.reorder ~move_before:"x" ["m"] [cTypDef "obj"];      
)
