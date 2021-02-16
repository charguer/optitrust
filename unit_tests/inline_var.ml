open ScriptTools


let _ = run
   ( fun _ ->
   set_init_source"inline_var.cpp";
   inline_decl ~delete_decl:false ~decl_path:[cVarDef ~name:"a" ()] ();
   inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"c" ()] ();
   inline_decl ~delete_decl:false ~decl_path:[cVarDef ~name:"x" ()] ();
   inline_decl ~delete_decl:false ~decl_path:[cVarDef ~name:"z" ()] ();
  )

