open Optitrust

let _ = 
    run 
    (fun _ ->
        set_init_source "fold_typ.cpp";
        fold_decl ~decl_path:[cTypDef "uint"] ();
        fold_decl ~decl_path:[cTypDef "cdouble"] (); 
        fold_decl ~decl_path:[cTypDef "mat2d"] ();
        (*
            fold_decl ~decl_path:[cTypDef "mat3d"()] (); 
            bad heap allocation error
         *)
        
        dump ()
    
    )