open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   !! ();
   !! Variable.inline ~delete_decl:false [cVarDef "a"];
   !! Variable.inline ~delete_decl:true [cVarDef "c"];
   !! Variable.inline ~delete_decl:false [cVarDef "x"];
   !! Variable.inline ~delete_decl:true [cVarDef "z"];
)
