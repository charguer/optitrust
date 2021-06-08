open Optitrust
open Run
let _ =
    run
    ( fun _ -> 
        set_init_source"insert_const.cpp";
        let vect_def= [cTypDef "vect"] in
        Declaration ~const:true [cAfter;vect_def] ~name:"NB_VECTS" ~value:"100" ();
        dump()
    )