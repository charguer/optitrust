open Optitrust
open Run
let _ = run
    (fun () ->
    set_init_source("extract_loop_var.cpp");
    Loop.hoist "x_step" [cFor "i"  ~body:[cVarDef "x"]()];
    dump ()    
    )


