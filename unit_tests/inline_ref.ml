open Optitrust
open Run

let _ =
    run
    ( fun _ ->
        set_init_source"inline_ref.cpp";
        Declaration.inline ~delete_decl:true [cVarDef "y"] ();
        (* 
            Does not work for references
        *)
        dump()

    )