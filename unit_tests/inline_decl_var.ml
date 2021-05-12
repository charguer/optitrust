open Optitrust


let _ = run
   ( fun _ ->
   set_init_source"inline_decl_var.cpp";
   inline_decl ~delete_decl:false ~decl_target:[cVarDef "a"] ();
   inline_decl ~delete_decl:true ~decl_target:[cVarDef "c"] ();
   inline_decl ~delete_decl:false ~decl_target:[cVarDef "x"] ();
   inline_decl ~delete_decl:false ~decl_target:[cVarDef "z"] ();
   dump()
  )

