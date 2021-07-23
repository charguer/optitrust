open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Typedef.insert "typedef struct {int x; int y} vect" [tBefore;cTopFun "main"];
    !! Typedef.insert "typedef struct {vect speed; vect pos} particle" [tBefore;cTopFun "main"];

)

(* LATER:

Typdef.insert "myvect" (typedef_alias (typ_constr "vect" []))
Typdef.insert "vect" (typedef_record [("x", typ_int); ("y", typ_int)])

let define_vect nbDimension typ =
  let dims = List.inti nbDimension (fun i -> ["dim" ^ string_of_int i, typ]) in
  Typdef.insert "vect" (typedef_record dims)

  >> the point is one day we will have smart constructors
      e.g. typedef_alias
*)