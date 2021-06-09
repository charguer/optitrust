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
      (* TODO: This is not yet implemented *)
      inline_decl ~delete_decl:false ~decl_target:[cFunDef "v_add"] ();
      inline_decl ~delete_decl:false ~decl_target:[cTopFun "g"] ();
      inline_decl ~delete_decl:true ~decl_target:[cVarDef "res"]();
      (* inline_decl ~delete_decl:true ~decl_target:[cVarDef "x"] ~inline_at:[[cFunDef "main"];[cSet ~lhs:[cVar "y"]()]] (); *)
      dump ()
    )

