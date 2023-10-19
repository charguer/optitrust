open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   (* !! Variable.bind_multi ~dest:[tBefore; cFor "i"] "a" [sExpr "2 + 3"]; *)
   !! Variable.bind_multi ~dest:[tBefore; cFor "j"] "b" [sExpr "2 + s"];
   (* FIXME: need to a transfo to do this correctly.
   !! Variable.bind_multi ~dest:[tBefore; cVarDef "r"] "c" [sExpr "4 +"];
   *)
)
