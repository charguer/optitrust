open ScriptTools

let _ = 
  run
  (fun _ ->
  set_init_source"tile_array.cpp";
  tile_array  ~block_name:"X"  ~block_size:"B" "T";
  tile_array  ~block_name:"Y" ~block_size:"B" "U";
  dump()
  
  )

