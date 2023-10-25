open Optitrust
open Target
open Prelude

let _ = Run.doc_script_cpp (fun _ ->

  !! Typedef_basic.insert "uchar" (Typdef_alias (ty "unsigned char" )) [tAfter; cVarDef "a"];

)

"
int a = 0;
"

(* LATER: a combi version: Typedef.insert_alias, and same for insert_struct *)

let _ = Run.script_cpp (fun _ ->

    let td = typdef_record [(Record_field_member ("x", typ_int()), Access_unspecified); (Record_field_member ("y", typ_int()), Access_unspecified)] in
    !! Typedef_basic.insert "vect" td [tAfter; cVarDef "M"];
    !! Typedef_basic.insert "myvect" (Typdef_alias (ty "vect" )) [tAfter; cTypDef "vect"];

    (* NOTE: see also [sequence_insert.ml] for inserting plain code *)

    (* ARTHUR :think about the policy for maintaining IDs..*)

)
