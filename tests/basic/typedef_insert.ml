open Optitrust
open Target
let _ =
    run
    (fun _ ->
        set_init_source"insert_type.cpp";
        let main_fun= [cFunDef "main"] in
        Declaration.insert_typedef [tBefore;main_fun] ~name:"T" ~value:"T[M][N]" ();
        dump()
    ) 