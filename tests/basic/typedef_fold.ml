open Optitrust
open Target
(* TODO: Fix the issue when folding mat3d *)
let _ = Run.script_cpp (fun _ ->
  !! Typedef.fold [cTypDef "uint"];
  !! Typedef.fold [cTypDef "cdouble"]; 
  !! Typedef.fold [cTypDef "mat2d"] ;
  !! Typedef.fold [cTypDef "mat3d"] ;
  )
  