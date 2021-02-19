open ScriptTools

(*
    TODO: Implmeent pretty struct into struct inlining
*)


let _ = 
    run 
    ( fun _ -> 
        set_init_source"inline_struct_in_struct.cpp";
        inline_decl ~delete_decl:true ~decl_path:[cType ~name:"vect" ()] ();
        dump()
    )