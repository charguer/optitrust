open Optitrust
open Run
let _ = run
    (fun _ ->
        set_init_source"tile_loop.cpp";
        Loop.tile [cFor ""];
        dump()
    )
(*
// NOTE: the current implementation of loop tiling removes 'int i' in case
// it is not used in the loop.
*)