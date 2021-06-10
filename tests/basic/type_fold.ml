open Optitrust
open Target

(* TODO: Does not work*)
let _ = Run.script_cpp (fun _ ->
        (* Declaration.fold [cTypDef "uint"]; *)
        Generic.target_show [cTypDef "uint"];
        Declaration.fold [cTypDef "cdouble"]; 
        Declaration.fold [cTypDef "mat2d"] ;
    )