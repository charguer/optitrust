open Optitrust
open Run

let _ = run
   ( fun _ ->
   set_init_source"inline_decl_var.cpp";
   Declaration.inline ~delete_decl:false [cVarDef "a"] ();
   Declaration.inline ~delete_decl:true [cVarDef "c"] ();
   Declaration.inline ~delete_decl:false [cVarDef "x"] ();
   Declaration.inline ~delete_decl:false [cVarDef "z"] ();
   dump()
  )

