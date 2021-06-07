open Optitrust
open Run

let _ = run
(fun _ ->
  set_init_source"remove_decl.cpp";
  Declaration.remove [cTypDef "T"] ();
  Declaration.remove [cTopFun "f"] ();
  (* TODO: this one does not work
       remove_decl ~decl_path:[cVarDef "x"] ();
     When it does, update the %_exp.cpp file *)
  Declaration.remove [cVarDef "z"] ();
  dump ()
)

