open Optitrust

let _ = 
  run 
  ( fun () ->
    set_init_source "test_pic_1/test_pic_1.cpp";
    (*inline_decl ~delete_decl:false ~inline_at:[[cTypDef "pos"];[cVar ~name:"speed" ()]]~decl_path:[cTypDef~name:"vect" ()] ();
    *)
    inline_decl ~delete_decl:false ~decl_path:[cTypDef~name:"vect" ()] ();

    inline_decl ~delete_decl:false ~decl_path:[cTypDef~name:"particle" ()] ();
    inline_seq ~seq_path:[cTypDef "pos";cTypDef "speed"] ();
      
    dump()
  )