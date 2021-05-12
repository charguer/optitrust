open Optitrust

let _ =
    run
    (fun _ ->
        set_init_source"split_seq.cpp";
        (* show_path [cVarDef "x"] ~debug_ast:true; *)
        let name_split = fun x -> "t" in 
        split_sequence ~keep_labels:true ~split_name:name_split [cStr "y++"];
        show_path [cVarDef "t"] ~debug_ast:true;
        dump()
         
    )