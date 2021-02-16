open ScriptTools

let _ = run
    (fun _ ->
        set_init_source"swap_coords_fixed.cpp";
        swap_coordinates "T";
        dump()
    )