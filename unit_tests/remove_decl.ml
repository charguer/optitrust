open Optitrust

let _ = run
(fun _ ->
  set_init_source"remove_decl.cpp";
  remove_decl ~decl_path:[cTypDef "T"] ();
  remove_decl ~decl_path:[cTopFun "f"] ();
  (* TODO: this one does not work
       remove_decl ~decl_path:[cVarDef "x"] ();
     When it does, update the %_exp.cpp file *)
  remove_decl ~decl_path:[cVarDef "z"] ();
)

