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
      fold_at: targets where folding should be performed, if left empty then folding 
        on all the nodes of the same sequence t belongs to.
      t: ast of the typedef declaration
    return:
      update ast
*)
let fold_aux (fold_at : target) (index : int) (t : trm) : trm=
  match t.desc with
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let d, lback = Tools.split_list_at 1 lback in
    let d = List.hd d in
    begin match d.desc with
     | Trm_typedef td ->
       begin match td.typdef_body with
       | Typdef_alias dx ->
        let ty_x = typ_constr td.typdef_tconstr  td.typdef_typid [] in
        let lback = List.map (Internal.change_typ ~change_at:[fold_at] dx ty_x) lback in
        trm_seq ~annot:t.annot (lfront @ [d] @ lback)
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
      delete: a flag for deciding if we should delete or not the typedef 
        declaration
      inline_at: targets where inlining should be performed, if empty inlining is applied
        on all the ast nodes in the same level as the typedef declaration
      t: ast subterm
    return:
      updated ast
*)
let inline_aux (delete : bool) (inline_at : target) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let dl, lback = Tools.split_list_at 1 lback in
    let dl = List.hd dl in
    begin match dl.desc with
    | Trm_typedef td ->
     begin match td.typdef_body with
     | Typdef_alias dx ->
      let ty_x = typ_constr td.typdef_tconstr td.typdef_typid [] in
      let lback = List.map(Internal.change_typ ~change_at:[inline_at] ty_x dx) lback in
      let tl =
        if delete then lfront @ lback
        else lfront @ [dl] @ lback
      in
      trm_seq ~annot:t.annot tl
     | _ -> fail t.loc "inline_aux: expected a typdef_alias"
     end
    | _ -> fail t.loc "inline_aux: expected a typedef declaration"
    end
  | _ -> fail t.loc "inline_aux: expected the surrounding sequence"

(* [inline delete inline_at index t p] *)
let inline (delete : bool) (inline_at : target) (index : int) : Target.Transfo.local =
  Target.apply_on_path (inline_aux delete inline_at index)

(* [alias_aux name index t]: create a copy of a typedef with a new name
    params:
      name: new typ name
      index: index of the original typedef declaration in the sequence it belongs to to
      t: ast of the surrounding sequence of the original declaration
    return:
      updated surrounding sequence with added new copy of the original declaration
*)
let alias_aux (name : string) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, td_l, lback = Internal.get_trm_and_its_relatives index tl in
    let td_copy = match td_l.desc with 
    | Trm_typedef td ->
      trm_typedef {td with typdef_tconstr = name}
    | _ -> fail t.loc "alias_aux: expected a typedef declaration" 
     in
      trm_seq ~annot:t.annot (lfront @ [td_l] @ [td_copy] @ lback)

  | _-> fail t.loc "alias_aux: expected the surrounding sequence"

let alias (name : string) (index : int) : Target.Transfo.local =
  Target.apply_on_path (alias_aux name index)
