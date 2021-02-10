open ScriptTools

let _ =
  run
    (fun _ ->
      set_init_source "test_pic_3.cpp";
      
      (* 
            let main_fun = [cFun ~name:"main" ()] in 
      
      insert_const ~insert_before:main_fun ~name:"nbColors" ~value:"8" ();
      insert_const ~insert_before:main_fun ~name:"nbCellsPerTile" ~value:"8" ();
      insert_const ~insert_before:main_fun ~name:"nbCellsPerColor" ~value:"nbCells / nbColors" ();
      insert_const ~insert_before:main_fun ~name:"nbTiles" ~value:"nbCells / nbCellsPerTile" ();
      insert_const ~insert_before:main_fun ~name:"nbTilesPerColor" ~value:"nbCells / nbCellsPerTile" ();
      insert_decl ~insert_before:main_fun ~name:"idCellsOfTile" ~value:"[nbTiles][nbCellsPerTile]"();
      
      *)
   
      
    
      let insert_before= [cVarDef ~name:"field" ()] in
      (*Substitute idTile with temp, don't know how name it'*)
      insert_decl ~insert_before ~name:"temp" ~value:"idCell / nbCellsPerTile" ();
      insert_and_fold ~insert_before ~name:"idCellOfTile" ~value:"idCell % nbCellsPerTile" ();
      tile_loop [cFor ~init:[cVarDef ~name:"idCell" ()] ()];
      fold_decl ~decl_path:[cVarDef ~name:"nbTiles" ()] ();
      let insert_before = [cFor ~init:[cVarDef ~name:"idCellOfTile" ()] ()] in 
      insert_and_fold ~insert_before ~name:"color" ~value:"temp / nbTiles"();
      insert_decl ~insert_before ~name:"idTile" ~value:"temp % nbTiles" ();
      tile_loop [cFor ~init:[cVarDef ~name:"temp" ()] ()];
      
      dump ()
    )

