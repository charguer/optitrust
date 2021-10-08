open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    !! Struct_basic.rename_fields (fun x -> x ^ "_rel") [cTypDef "vect"];
    !! Struct_basic.rename_fields (fun x -> x ^ "_rel") [cTypDef "obj"];
)
