open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    !! Struct_basic.rename_fields (fun x -> "rel_" ^ x) [cTypDef "vect"];
    !! Struct_basic.(rename_fields Rename.(only_for "pos" (fun x ->  "rel_" ^ x))) [cTypDef "obj"];
    !! Struct_basic.(rename_fields Rename.(only_for "speed" (fun x ->  "rel_" ^ x))) [cTypDef "obj"];
)
