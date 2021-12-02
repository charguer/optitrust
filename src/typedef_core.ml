open Ast
open Target

(* *********************************************************************************** 
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [fold_aux as_reference fold_at]: replace all the occurrences of the typedef underlying type
      with the defined type
    params:
      [fold_at]: targets where fold_lefting should be performed, if left empty then fold_lefting 
        on all the nodes of the same sequence t belongs to.
      [t]: ast of the typedef declaration
    return:
      the updated ast with the folded type declared at [t]
*)
let fold_aux (fold_at : target) (index : int) (t : trm) : trm=
  match t.desc with
  | Trm_seq tl ->
    let lfront, d, lback = Internal.get_trm_and_its_relatives index tl in
    begin match d.desc with
     | Trm_typedef td ->
       begin match td.typdef_body with
       | Typdef_alias dx ->
        let ty_x = typ_constr td.typdef_tconstr  ~tid:td.typdef_typid  in
        let lback = Mlist.map (Internal.change_typ ~change_at:[fold_at] dx ty_x) lback in
        let new_tl = Mlist.merge lfront lback in
        let new_tl = Mlist.insert_at index d new_tl in
        trm_seq ~annot:t.annot ~marks:t.marks new_tl
       | _ -> fail t.loc "fold_decl: expected a typedef"
       end
     | _ -> fail t.loc "fold_decl: expected a type definition"
     end


  | _ -> fail t.loc "fold_aux: expected the surrounding sequence"

let fold (fold_at : target) (index) : Target.Transfo.local =
  Target.apply_on_path(fold_aux fold_at index)


(* [inline_aux inline_at]: replace all the occurrences of the defined type with
        its underlying type
    params:
      [delete]: a flag for deciding if we should delete or not the typedef declaration
      [inline_at]: targets where inlining should be performed, if empty inlining is applied
        on all the ast nodes in the same level as the typedef declaration
      [t]: ast subterm
    return:
      updated ast with the typedef occurrences inlined
*)
let inline_aux (delete : bool) (inline_at : target) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, dl, lback = Internal.get_trm_and_its_relatives index tl in
    begin match dl.desc with
    | Trm_typedef td ->
     begin match td.typdef_body with
     | Typdef_alias dx ->
      let ty_x = typ_constr td.typdef_tconstr ~tid:td.typdef_typid  in
      let lback = Mlist.map(Internal.change_typ ~change_at:[inline_at] ty_x dx) lback in
      let tl = Mlist.merge lfront lback in
      let new_tl = 
      if delete then tl else Mlist.insert_at index dl tl
      in
      trm_seq ~annot:t.annot ~marks:t.marks new_tl
     | _ -> fail t.loc "inline_aux: expected a typdef_alias"
     end
    | _ -> fail t.loc "inline_aux: expected a typedef declaration"
    end
  | _ -> fail t.loc "inline_aux: expected the surrounding sequence"


let inline (delete : bool) (inline_at : target) (index : int) : Target.Transfo.local =
  Target.apply_on_path (inline_aux delete inline_at index)

(* [insert_copy_aux name index t]: create a copy of a typedef with a new name
    params:
      [name]: new typ name
      [t]: ast of the surrounding sequence of the original declaration
    return:
      the updated sequence with the newly inserted typedef with the smae structure ast the one
          at index [index] but name [name]
*)
let insert_copy_aux (name : string) (t : trm) : trm =
  match t.desc with 
  | Trm_typedef td ->
    let td_copy = trm_typedef {td with typdef_tconstr = name} in
    trm_seq_no_brace [t; td_copy]
  | _ -> fail t.loc "insert_copy_aux: expected a typedef declaration" 

let insert_copy (name : string) : Target.Transfo.local =
  Target.apply_on_path (insert_copy_aux name )

(* [insert_aux name td_body index]: insert a new type definition
    params:
      [name]: new type name
      [td_body]: body of the new type definition
      [index]: location where the typedef should be inserted inside a sequence
    return:
      updated surrounding sequence with added typedef definition
*)
let insert_aux (name : string) (td_body : typdef_body) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
     let tid = next_typconstrid () in
     let trm_to_insert = trm_typedef {typdef_typid = tid; typdef_tconstr = name; typdef_body = td_body;typdef_vars = [];typdef_loc = None} in
     let new_tl = Mlist.insert_at index trm_to_insert tl in
     trm_seq ~annot:t.annot ~marks:t.marks new_tl
  | _ -> fail t.loc "insert_aux: expected the surrounding sequence"

let insert (name : string) (td_body : typdef_body) (index : int) : Target.Transfo.local =
  Target.apply_on_path (insert_aux name td_body index)



