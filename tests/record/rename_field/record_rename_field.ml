open Optitrust
open Target



let _ = Run.script_cpp (fun _ ->

    !! Record.rename_field "pos" ~into:"rel_pos" [cTypDef "obj"];
    !! Record.rename_field "speed" ~into:"rel_speed" [cTypDef "obj"];
)
