open ScriptTools

let _ = run
    (fun _ -> 
        set_init_source"swap_coords_vari.cpp";
        (**Error
            Fatal error: exception (Failure "swap_type: must be an array")
        *)
        swap_coordinates "t";
        dump()
    )