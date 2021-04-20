open Optitrust

let _ = run
    (fun () ->
    set_init_source("extract_loop_var.cpp");
    extract_loop_var [cFor ~init:[cVarDef ~name:"i" ()] ~body:[cVarDef ~name:"x"()]()];
    dump ()    
    )