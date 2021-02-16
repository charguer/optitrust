open ScriptTools

let _ =
    run
    ( fun _ ->
        set_init_source "split_loop.cpp";
        split_loop_nodep ~keep_labels:false [cFor ~init:[cVarDef ~name:"i" ()] ()];
        dump()
    )

(* // note: the transformation introduces labels, let's make the script clean all labels *)
