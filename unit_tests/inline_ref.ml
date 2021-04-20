open Optitrust

let _ =
    run
    ( fun _ ->
        set_init_source"inline_ref.cpp";
        inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"y" ()] ();
        (* 
            Does not work for references
        *)
        dump()

    )