(* option: use option to add open at compile time *)
open ScriptTools

let _ =
  run
    (fun () ->
      set_init_source "test_suite/test_swap_coordinates.cpp";
      add_label "return_instr" [cInstrSubstr "return"];
      swap_coordinates "T";
      add_label "for_loop" [cFor ()];
      dump ();
      reset ();
      set_init_source "test_suite/test_split.cpp";
      split_loop ~keep_labels:true [cInstrSubstr ~regexp:true "^x ="];
      dump ();
      reset ();
      set_init_source "test_suite/test_array_tiling.c";
      tile_array ~block_size:"2" "T";
      tile_array ~name:(fun x -> x ^ "_Stiled") ~block_size:"2" "S";
      let insert_before = [cSet ~rhs:[cStr "(2 * i)"] ()] in
      insert_and_fold ~insert_before ~name:"i1" ~value:"(i / 2)" ();
      insert_and_fold ~insert_before ~name:"i2" ~value:"(i % 2)" ();
      tile_loop [cFor ()];
      dump ()
    )
