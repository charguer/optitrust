open Optitrust

let _ = 
    run 
    (fun _ ->
        set_init_source "fold_typ.cpp";
        fold_decl ~decl_path:[cType ~name:"uint"()] ();
        fold_decl ~decl_path:[cType ~name:"cdouble"()] (); 
        fold_decl ~decl_path:[cType ~name:"mat2d" ()] ();
        (*
            fold_decl ~decl_path:[cType ~name:"mat3d"()] (); 
            bad heap allocation error
         *)
        
        dump ()
    
    )