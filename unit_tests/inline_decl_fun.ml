open ScriptTools
(*
let _ = run_unit_test (fun () ->
   inline_decl ~delete_decl:true ~decl_path:[cTopFun ~name:"f" ()] ();
  )

 The above is a shorthand for: *)
let _ =
  run
    (fun () ->
      set_init_source "inline_decl_fun.cpp";
      
      inline_decl ~delete_decl:false ~decl_path:[cTopFun ~name:"f" ()] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"res"()]();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"x"()] ~inline_at:[[cFun ~name:"main" ()];[cSet ~lhs:[cVar ~name:"y"()]()]] ();
      show_path [cVarDef ~name:"y"()] ~debug_ast:true;
      (* show_path [cLabel ~label:"exit" ();cBody()] ~debug_ast:true; *)
      dump ()
    )

