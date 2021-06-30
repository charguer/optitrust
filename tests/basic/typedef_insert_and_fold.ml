open Optitrust
open Target

(* TODO: Fix the issue when folding mat3d *)

let _ = Run.script_cpp (fun _ ->
  !! Typedef.insert_and_fold "typedef unsigned int uint" [tBefore; cTypDef "vect"];
  !! Typedef.insert_and_fold "typedef double const cdouble" [tBefore; cTypDef "vect"];
  !! Typedef.insert_and_fold "typedef int** mat2d" [tAfter; cTypDef "vect"];
  !! Typedef.insert_and_fold "typedef int*** mat3d" [tAfter; cTypDef "vect"];
)
  