open Optitrust
open Run
let _ = 
    run 
    (fun _ ->
        set_init_source "fold_typ.cpp";
        Declaration.fold [cTypDef "uint"] ();
        Declaration.fold [cTypDef "cdouble"] (); 
        Declaration.fold_decl [cTypDef "mat2d"] ();
        dump ()
    
    )