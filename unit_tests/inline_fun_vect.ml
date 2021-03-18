open ScriptTools
(*
let _ = run_unit_test (fun () ->
   inline_decl ~delete_decl:true ~decl_path:[cTopFun ~name:"f" ()] ();
  )

 The above is a shorthand for: *)
let _ =
  run
    (fun () ->
      set_init_source "inline_fun_vect.cpp";
      inline_decl ~delete_decl:false ~decl_path:[cFun ~name:"g" ()] ();
      dump ()
    )

