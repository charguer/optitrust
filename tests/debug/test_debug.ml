open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

   
   !! Rewrite_basic.equiv_at ~glob_defs:"double fmap(double, double);\n" "double x, y, z; ==> (fmap(x,y)/z) == (fmap(x/z, y/z))" [cVarDef "p"; cInit()];
   
)
 