open Optitrust
open Target
open Path


let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ -> 

   show [cVarDef ""; cInit];
   !! Rewrite_basic.equiv_at "int x, k, l; ==> k + x * l == l * x + k" [cVarDef "b"; cInit];

)
 