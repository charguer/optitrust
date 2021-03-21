open ScriptTools
(*
let _ = run_unit_test (fun () ->
   inline_decl ~delete_decl:true ~decl_path:[cTopFun ~name:"f" ()] ();
  )

 The above is a shorthand for: *)
let _ =
  run
    (fun () ->
      set_init_source "inline_fun_vect_copy.cpp";
        
      inline_decl ~delete_decl:false ~decl_path:[cTopFun ~name:"f" ()] ();
(*inline_decl ~delete_decl:false ~decl_path:[cTopFun ~name:"g" ()] ();*)
inline_decl ~delete_decl:false ~decl_path:[cTopFun ~name:"h" ()] ();
inline_decl ~delete_decl:false ~decl_path:[cTopFun ~name:"add" ()] ();      
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"res" ()] ();
      dump ()
    )

