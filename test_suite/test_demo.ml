open Optitrust

let _ = 
  run 
    (fun () ->
      set_init_source "test_demo/test_demo.cpp";
      dump();
      (*
      inline_decl ~delete_decl:false  ~decl_path:[cTopFun "foo"] ();
      inline_decl ~delete_decl:false  ~decl_path:[cTopFun "vect_add"] ();
      *)
      
      dump ()
      )