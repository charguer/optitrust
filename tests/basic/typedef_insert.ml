open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    !! Typedef_basic.insert "vect" (Typdef_prod (false,[("x", Ast.typ_int () ); ("y", Ast.typ_int ())])) [tAfter;cVarDef "M"];
    !! Typedef_basic.insert "myvect" (Typdef_alias (Ast.typ_constr "vect" (-1) [])) [tAfter;cTypDef "vect"]; 
)
