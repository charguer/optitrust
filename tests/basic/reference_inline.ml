open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Variable_basic.inline [cVarDef "y"];
  !! Variable_basic.inline ~delete_decl:true [cVarDef "a"];
  !! Variable_basic.inline ~delete_decl:false [cVarDef "b"];
  !! Variable_basic.inline ~delete_decl:true [cVarDef "v"];
)

(* Remark: reference inlining is always correct! *)

(* TODO: ~at *)