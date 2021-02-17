open ScriptTools
let _ =
  run
    (fun _ ->
      set_init_source "testAOSOA/aos.cpp";
      let particle_typedef = [cType ~name:"particle" ()] in
      insert_const ~insert_before:particle_typedef ~name:"BLOCK_SIZE" ~value:"1 << 10" ();
      insert_const ~insert_before:particle_typedef ~name:"NB_BLOCKS" ~value:"NUM_PARTICLES / BLOCK_SIZE" ();
      insert_and_fold_typedef ~insert_after:particle_typedef ~name:"particle_array" ~value:"particle *" ();
      tile_array ~block_name:"particle_block" ~block_size:"BLOCK_SIZE" "particle_array";
      inline_decl ~delete_decl:true ~decl_path:[cType ~name:"particle_array" ()] ();
      let insert_before = [cSet ~lhs:[cAccesses ~accesses:[cAccess; cAccess; cField ~field:"x" ()] ()] ()] in
      insert_and_fold ~insert_before ~name:"i1" ~value:"i / BLOCK_SIZE" ();
      insert_and_fold ~insert_before ~name:"i2" ~value:"i % BLOCK_SIZE" ();
      tile_loop [cFor ~init:[cVarDef ~name:"i" ()] ()];
      fold_decl ~decl_path:[cVarDef ~name:"NB_BLOCKS" ()] ();
      switch
        [
          (fun _ -> ());
          (fun _ ->
            add_attribute "ALIGNED" [cType ~name:"particle_block" ()];
            add_attribute "ALIGNED" [cVarDef ~name:"data" ()]
          )
        ];
      aos_to_soa "particle_block";
      remove_decl ~decl_path:[cType ~name:"particle" ()] ();
      let i2_loop = [cFor ~init:[cVarDef ~name:"i2" ()] ()] in
      insert_and_fold ~insert_before:i2_loop ~as_reference:true ~fold_at:[i2_loop] ~name:"b" ~value:"data[i1]" ();
      inline_seq ~seq_path:[[cVarDef ~strict:true ~name:"b" ()] >>! []] ();
      inline_decl ~delete_decl:true ~decl_path:[cTopFun ~name:"my_alloc" ()] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"nb_elts" ()] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"size_elt" ()] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"res" ()] ();
      inline_seq ~seq_path:[[cVarDef ~name:"mode" ()] >>! []] ();
      dump ()
    )
