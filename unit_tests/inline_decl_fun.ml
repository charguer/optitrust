open Optitrust
(*
let _ = run_unit_test (fun () ->
   inline_decl ~delete_decl:true ~decl_path:[cTopFun ~name:"f" ()] ();
  )

 The above is a shorthand for: *)
let _ =
  run
    (fun () ->
      set_init_source "inline_decl_fun.cpp";
      (* detach_expression [cVarDef :"v3"] ~keep_label:false; *)
      inline_decl ~delete_decl:false ~decl_path:[cFunDef "v_add"] ();
      inline_decl ~delete_decl:false ~decl_path:[cTopFun ~name:"g" ()] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef "res"]();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef "x"] ~inline_at:[[cFunDef ~name:"main" ()];[cSet ~lhs:[cVar "y"]()]] ();
      show_path [cVarDef "y"] ~debug_ast:true;
      (* show_path [cLabel ~label:"exit" ();cBody()] ~debug_ast:true; *)
      dump ()
    )

