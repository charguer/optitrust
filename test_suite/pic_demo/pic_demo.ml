open Optitrust

let _ = 
  run
    ( fun () ->
      set_init_source "pic_demo.cpp";
      
      dump()
    )