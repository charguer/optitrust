open Optitrust
open Run

(* TODO: Does not work*)
let _ = run_unit_test (fun _ ->
        (* Declaration.fold [cTypDef "uint"]; *)
        Generic.target_show [cTypDef "uint"];
        Declaration.fold [cTypDef "cdouble"]; 
        Declaration.fold [cTypDef "mat2d"] ;
    )