open Optitrust
open Target

let _ = Run.script_cpp( fun _ ->
  !! Typedef_basic.inline ~at:[cTypDef "vect"] [cTypDef "uint"];
  !! Typedef_basic.inline ~at:[cFunDef "f"] [cTypDef "cdouble"];
  !! Typedef_basic.inline [cTypDef "mat2d"];
  !! Typedef_basic.inline [cTypDef "mat3d"];
)
