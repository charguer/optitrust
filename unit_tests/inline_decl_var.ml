open Optitrust


let _ = run
   ( fun _ ->
   set_init_source"inline_decl_var.cpp";
   inline_decl ~delete_decl:false ~decl_path:[cVarDef "a"] ();
   inline_decl ~delete_decl:true ~decl_path:[cVarDef "c"] ();
   inline_decl ~delete_decl:false ~decl_path:[cVarDef "x"] ();
   inline_decl ~delete_decl:false ~decl_path:[cVarDef "z"] ();
   dump()
  )

