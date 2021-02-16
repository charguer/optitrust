open ScriptTools
let _ = run 
    (fun _ -> 
        set_init_source"tile_loop.cpp";
        tile_loop [cFor ()];
        dump()
    )