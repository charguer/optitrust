open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Variable.inline [cVarDef "y"];
  !! Variable.inline ~delete_decl:true [cVarDef "a"];
  !! Variable.inline ~delete_decl:false [cVarDef "b"];
  !! Variable.inline ~delete_decl:true [cVarDef "v"];
)

(* Remark: reference inlining is always correct! *)

(* TODO: ~at *)