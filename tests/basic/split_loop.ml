open Optitrust
open Run

let _ =
    run
    ( fun _ ->
        set_init_source "split_loop.cpp";
        Loop.split [cFor "i"];
        dump()
    )
