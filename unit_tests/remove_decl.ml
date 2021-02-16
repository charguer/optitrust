open ScriptTools

let _ = run_unit_test (fun () ->
  remove_decl ~decl_path:[cType ~name:"T" ()] ();
  remove_decl ~decl_path:[cTopFun ~name:"f" ()] ();
  (* TODO: this one does not work
       remove_decl ~decl_path:[cVarDef ~name:"x" ()] ();
     When it does, update the %_exp.cpp file *)
  remove_decl ~decl_path:[cVarDef ~name:"z" ()] ();
)

