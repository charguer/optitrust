open Optitrust
let _ =
  run
    (fun _ ->
      set_init_source "test_aosoa.cpp";
      let particle_typedef = [cTypDef "particle"] in
      insert_const ~insert_before:particle_typedef ~name:"BLOCK_SIZE" ~value:"1 << 10" ();
      insert_const ~insert_before:particle_typedef ~name:"NB_BLOCKS" ~value:"NUM_PARTICLES / BLOCK_SIZE" ();
      insert_and_fold_typedef ~insert_after:particle_typedef ~name:"particle_array" ~value:"particle *" ();
      tile_array ~block_name:"particle_block" ~block_size:"BLOCK_SIZE" "particle_array";
      inline_decl ~delete_decl:true ~decl_path:[cTypDef "particle_array"] ();
      let insert_before = [cSet ~lhs:[cAccesses ~accesses:[cAccess; cAccess; cField ~field:"x" ()] ()] ()] in
      insert_and_fold ~insert_before ~name:"i1" ~value:"i / BLOCK_SIZE" ();
      insert_and_fold ~insert_before ~name:"i2" ~value:"i % BLOCK_SIZE" ();
      tile_loop [cFor "i"];
      fold_decl ~decl_path:[cVarDef "NB_BLOCKS"] ();
      switch
        [
          (fun _ -> ());
          (fun _ ->
            add_attribute "ALIGNED" [cTypDef "particle_block"];
            add_attribute "ALIGNED" [cVarDef "data"]
          )
        ];
      aos_to_soa "particle_block";
      remove_decl ~decl_path:[cTypDef "particle"] ();
      let i2_loop = [cFor "i2"] in
      insert_and_fold ~insert_before:i2_loop ~as_reference:true ~fold_at:[i2_loop] ~name:"b" ~value:"data[i1]" ();
      inline_seq ~seq_path:[[cVarDef "b"] >>! []] ();
      inline_decl ~delete_decl:true ~decl_path:[cTopFun "my_alloc"] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef "nb_elts"] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef "size_elt"] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef "res"] ();
      inline_seq ~seq_path:[[cVarDef "mode"] >>! []] ();
      dump ()
    )
