open Optitrust
open Target


let _ = Run.script_cpp( fun _ ->

  !! Typedef_basic.unfold ~at:[cTypDef "vect"] [cTypDef "uint"];
  !! Typedef_basic.unfold ~at:[cFunDef "f"] [cTypDef "cdouble"];
  !! Typedef_basic.unfold [cTypDef "mat2d"];
  !! Typedef_basic.unfold [cTypDef "mat3d"];

)
