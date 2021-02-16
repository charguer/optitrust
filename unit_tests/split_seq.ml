open ScriptTools

let _ =
    run
    (fun _ ->
        set_init_source"split_seq.cpp";
        let name_split = fun x -> "t" in 
        split_sequence ~keep_labels:false ~split_name:name_split [cStr "y++"];
        dump()
         
    )