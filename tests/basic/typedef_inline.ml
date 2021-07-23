open Optitrust
open Target

let _ = Run.script_cpp( fun _ ->
  !! Typedef_basic.inline [cTypDef "uint"];
  !! Typedef_basic.inline [cTypDef "cdouble"];
  !! Typedef_basic.inline [cTypDef "mat2d"];
  !! Typedef_basic.inline [cTypDef "mat3d"];
)

(* TODO: nice to see demos with ~at: *)