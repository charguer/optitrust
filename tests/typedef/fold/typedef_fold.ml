open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Typedef_basic.fold ~at:[cTypDef "vect"] [cTypDef "ui"];
  !! Typedef_basic.fold [cTypDef "cdouble"]; (* Should do nothing: const does not exist after decoding *)
  !! Typedef_basic.fold [cTypDef "mat2d"];
  !! Typedef_basic.fold [cTypDef "mat3d"];

)
