open ScriptTools

(*
    TODO: Implmeent pretty struct into struct inlining
*)


let _ = 
    run 
    ( fun _ -> 
        set_init_source"inline_struct.cpp";
        inline_decl ~decl_path:[cType ~name:"vect" ()] ();
        dump()
    )
