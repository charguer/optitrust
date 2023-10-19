open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   !! Variable.bind_syntactic ~dest:[tBefore; cFor "i"] ~fresh_name:"a" [sExpr "2 + 3"];
   !! Variable.bind_syntactic ~dest:[tBefore; cFor "j"] ~fresh_name:"b" [sExpr "2 + s"];
   !! Variable.bind_syntactic ~dest:[tBefore; cVarDef "r"] ~fresh_name:"c${occ}" [sExpr "4 +"];
   !! Variable.bind_syntactic ~dest:[tBefore; cVarDef "x"] ~fresh_name:"x${occ}" [cArrayRead "m"];
)
