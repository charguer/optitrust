open Optitrust
open Target

let _ = Run.script_cpp ( fun _ -> 
  !! Variable.inline ~delete_decl:true [cVarDef "y"];
  !! Variable.inline ~delete_decl:true [cVarDef "a"];
  !! Variable.inline ~delete_decl:true [cVarDef "b"];
  !! Variable.inline ~delete_decl:true [cVarDef "v"];
)
    