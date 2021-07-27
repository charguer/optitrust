open Optitrust
open Target
(* TODO: Fix the issue when folding mat3d *)
let _ = Run.script_cpp (fun _ ->
  !! Typedef_basic.fold ~at:[cTypDef "vect"] [cTypDef "uint"]; 
  !! Typedef_basic.fold ~at:[cVarDef "y1"] [cTypDef "cdouble"];
  !! Typedef_basic.fold [cTypDef "mat2d"] ;
  !! Typedef_basic.fold [cTypDef "mat3d"];
  )

(* TODO: rename fold_at to at
   TODO: make ~at:target  instead of targetlist

  demo for:
     Typedef.fold ~at: [cTypDef "uint"];

    - example where you specify exactly one occurrence in a vardef
    - example where your replace all occurrences everywhere inside one given function
    - example where your replace all occurrences everywhere inside one given typdef

*)