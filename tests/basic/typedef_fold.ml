open Optitrust
open Target

(* Need to work on mat3d folding *)
let _ = Run.script_cpp (fun _ ->
        (* Declaration.fold [cTypDef "uint"]; *)
        Typedef.fold [cTypDef "uint"]; 
        Typedef.fold [cTypDef "cdouble"]; 

        Typedef.fold [cTypDef "mat2d"] ;
        (* Typedef.fold [cTypDef "mat3d"] ; *)
    )