open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

    let td = Typedef_record [(Record_field_member ("x", typ_int), Access_unspecified); (Record_field_member ("y", typ_int), Access_unspecified)] in
    !! Typedef_basic.insert "vect" td [tAfter; cVarDef "M"];
    !! Typedef_basic.insert "myvect" (Typedef_alias (ty "vect")) [tAfter; cTypDef "vect"];
    !!! ()

    (* NOTE: see also [sequence_insert.ml] for inserting plain code *)

)
