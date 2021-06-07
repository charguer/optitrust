open Optitrust
open Run
let _ =
    run
    (fun _ ->
        set_init_source"insert_type.cpp";
        let main_fun= [cFunDef "main"] in
        Declaration.insert_typedef [cBefore;main_fun] ~name:"T" ~value:"T[M][N]" ();
        dump()
    ) 