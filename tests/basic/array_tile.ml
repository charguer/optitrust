open Optitrust
open Run

let _ = 
  run
  (fun _ ->
  set_init_source"tile_array.cpp";
  Arrays.tile  ~block_name:"X"  ~block_size:"B" "T";
  Arrays.tile  ~block_name:"Y" ~block_size:"B" "U";
  dump()
  
  )

