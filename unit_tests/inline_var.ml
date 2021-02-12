open ScriptTools

let _ = run_unit_test (fun () ->
   inline_decl ~delete_decl:false ~decl_path:[cVarDef ~name:"a" ()] ();
   inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"c" ()] ();
   inline_decl ~delete_decl:false ~decl_path:[cVarDef ~name:"x" ()] ();
   inline_decl ~delete_decl:false ~decl_path:[cVarDef ~name:"z" ()] ();
  )

