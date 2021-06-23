open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
        (* Declaration.fold [cTypDef "uint"]; *)
        Typedef.fold [cTypDef "uint"]; 
        Typedef.fold [cTypDef "cdouble"]; 

        !!Typedef.fold [cTypDef "mat2d"] ;
    )