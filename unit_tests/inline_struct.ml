open ScriptTools

(*
    TODO: Implmeent pretty struct into struct inlining
*)


let _ = 
    run 
    ( fun _ -> 
        set_init_source"demo_inline_struct.cpp";
        inline_struct [cType ~name:"obj"()] "vect" ~struct_fields:["pos"];
        dump()
    )
