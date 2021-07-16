open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.smart_inline [cFun "g"];
  !! Variable.inline ~delete_decl:true [cVarDef "temp344"];
  !! Function.inline ~name_result:"r" [cFun "g"];
)