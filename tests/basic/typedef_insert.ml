open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    (* TODO: add a smart constructor    typedef_prod ?(recursive:bool=false) [ ... ] *)
    !! let td = Ast.Typdef_prod (false, [
        ("x", Ast.typ_int ());
        ("y", Ast.typ_int ())]) in
       Typedef_basic.insert "vect" td [tAfter; cVarDef "M"];
       (* TODO: add a smartconstructor called
             typ_constr_fresh name = typ_constr name (next_type_id()) *)

    !! let vect_id = -1 (* LATER: get_typ_constr_id "vect" *) in
       Typedef_basic.insert "myvect" (Typdef_alias (Ast.typ_constr "vect" vect_id [])) [tAfter; cTypDef "vect"];

    (* NOTE: see also [sequence_insert.ml] for inserting plain code *)

    (* ARTHUR :think about the policy for maintaining IDs..*)
)
