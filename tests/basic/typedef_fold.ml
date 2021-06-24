open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Typedef.fold [cTypDef "uint"];
  !! Typedef.fold [cTypDef "cdouble"]; (* TODO *)
  !! Typedef.fold [cTypDef "mat2d"] ;
  !! Typedef.fold [cTypDef "mat3d"] ;
  )