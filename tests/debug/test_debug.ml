open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

   !! Rewrite.equiv_at "double x; double y; double z; double t; ==> fwrap(x,y*z) == fwrap(x/t, (y*z)/t)" [nbMulti; cVarDef "p"; cInit ()];
   
)

(* LATER: at the combi level, combine struct_inline with struct-renaming-field *)
