open ScriptTools

let _ = 
  run 
  ( fun () ->
    set_init_source "test_pic_1/test_pic_1.cpp";
    (*inline_decl ~delete_decl:false ~inline_at:[[cType ~name:"pos"()];[cVar ~name:"speed" ()]]~decl_path:[cType~name:"vect" ()] ();
    *)
    inline_decl ~delete_decl:false ~decl_path:[cType~name:"vect" ()] ();

    inline_decl ~delete_decl:false ~decl_path:[cType~name:"particle" ()] ();
    inline_seq ~seq_path:[cType ~name:"pos" ();cType ~name:"speed" ()] ();
      
    dump()
  )