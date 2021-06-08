open Optitrust
open Run
let _ = 
    run
    (fun _ ->
        set_init_source"insert_var.cpp";
        let vect_def= [cTypDef "vect"] in
        Declaration.insert [cAfter;vect_def]  ~name:"size" ~value:"300" ();
        dump()
    )