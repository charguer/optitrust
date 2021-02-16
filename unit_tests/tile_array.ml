open ScriptTools

let _ = 
  run
  (fun _ ->
  tile_array ~block_name:"X" ~block_size:"B" "T";
  tile_array ~block_name:"Y" ~block_size:"B" "U";
  )

