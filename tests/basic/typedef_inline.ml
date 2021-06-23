open Optitrust
open Target

let _ = Run.script_cpp( fun _ ->
        Typedef.inline [cTypDef "uint"];
        Typedef.inline [cTypDef "cdouble"];
        Typedef.inline [cTypDef "mat2d"];
        Typedef.inline [cTypDef "mat3d"];
    )