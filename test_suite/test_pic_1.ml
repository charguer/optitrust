open ScriptTools

let _ = 
  run 
  ( fun () ->
    set_init_source "test_pic_1.cpp";
    inline_decl ~delete_decl:false ~decl_path:[cType~name:"vect" ()] ();
    inline_decl ~delete_decl:false ~decl_path:[cType~name:"particle" ()] ();
    
    dump()
  )