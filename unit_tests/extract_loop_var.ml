open Optitrust

let _ = run
    (fun () ->
    set_init_source("extract_loop_var.cpp");
    extract_loop_var [cFor "i"  ~body:[cVarDef "x"]()];
    dump ()    
    )