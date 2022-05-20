open Ast
open Target

(* [fold_aux fold_at index]: replaces occurrences of the typedef underlying type with the defined type,
       [fold_at] - targets where folding should be performed, if left empty then folding is applied recursively
                 on all the trms that belong to the same sequence as typedef,
       [index] - index of the typedef on its surrounding sequence,
       [t] - ast of the sequence that contains the targeted typedef. *)
let fold_aux (fold_at : target) (index : int) (t : trm) : trm=
  match t.desc with
  | Trm_seq tl ->
    let f_update (t : trm) : trm = t in
    let f_update_further (t : trm) : trm =
      let d = Mlist.nth tl index in 
      let dx, ty_x =
      begin match d.desc with 
      | Trm_typedef td -> 
        begin match td.typdef_body with 
        | Typdef_alias dx ->
           let ty_x = typ_constr td.typdef_tconstr ~tid:td.typdef_typid in 
           dx, ty_x
        | _ -> fail d.loc "Typedef_core.fold_aux: expected a type definition"
        end
      | _ -> fail d.loc "Typedef_core.fold_aux: expected a typedef"
      end in 

      Internal.change_typ ~change_at:[fold_at] dx ty_x t 
      in 
    let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Typedef_core.fold_aux: expected the surrounding sequence"

(* [fold fold_at index t p]: applies [fold_aux] at trm [t] with path [p]. *)
let fold (fold_at : target) (index) : Target.Transfo.local =
  Target.apply_on_path(fold_aux fold_at index)


(* [unfold_aux unfold_at]: replaces occurrences of the defined type with its underlying type,
      [delete] - a flag for deciding if we should delete or not the typedef declaration,
      [unfold_at] - targets where unfolding should be performed, if empty unfolding is applied
                   on all the trms that are with in the same level as the targeted typedef,
      [t] - ast of the sequence that contains the targeted typedef. *)
let unfold_aux (delete : bool) (unfold_at : target) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let f_update (t : trm) : trm = t in 
    let f_update_further (t : trm) : trm =
      let d = Mlist.nth tl index in 
      let dx, ty_x =
      begin match d.desc with 
      | Trm_typedef td ->
        begin match td.typdef_body with 
        | Typdef_alias dx -> 
          let ty_x = typ_constr td.typdef_tconstr ~tid:td.typdef_typid in 
          dx, ty_x
        | _ -> fail d.loc "Typedef_core.unfold_aux: expected a typedef alias"
        end
      | _ -> fail d.loc "Typedef_core.unfold_aux: expected a typdef_alias"
      end in 
      Internal.change_typ ~change_at:[unfold_at] ty_x dx t 
    in 
    let new_tl = Mlist.update_at_index_and_fix_beyond ~delete index f_update f_update_further tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Typedef_core.unfold_aux: expected the surrounding sequence"


(* [unfold delete unfold_at index]: applies [unfold] at trm [t] with path [p]. *)
let unfold (delete : bool) (unfold_at : target) (index : int) : Target.Transfo.local =
  Target.apply_on_path (unfold_aux delete unfold_at index)

(* [insert_copy_aux name index t]: creates a copy of the targeted typedef
    params:
      [name]: new typ name
      [t]: ast of the surrounding sequence of the targeted typedef *)
let insert_copy_aux (name : string) (t : trm) : trm =
  match t.desc with 
  | Trm_typedef td ->
    let td_copy = trm_typedef {td with typdef_tconstr = name} in
    trm_seq_no_brace [t; td_copy]
  | _ -> fail t.loc "Typedef_core.insert_copy_aux: expected a typedef declaration" 

(* [insert_copy name t p]: applies [insert_copy_aux] at trm [t] with path [p] *)
let insert_copy (name : string) : Target.Transfo.local =
  Target.apply_on_path (insert_copy_aux name )

(* [insert_aux name td_body index]: inserts a new type definition,
      [name] - new type name,
      [td_body] - body of the new type definition,
      [index] - location where the typedef should be inserted inside a sequence. *)
let insert_aux (name : string) (td_body : typdef_body) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
     let tid = next_typconstrid () in
     let trm_to_insert = trm_typedef {typdef_typid = tid; typdef_tconstr = name; typdef_body = td_body;typdef_vars = [];typdef_loc = None} in
     let new_tl = Mlist.insert_at index trm_to_insert tl in
     trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Typedef_core.insert_aux: expected the surrounding sequence"

(* [|insert_name td_body index]: applies [insert_aux] at trm [t] with path [p]. *)
let insert (name : string) (td_body : typdef_body) (index : int) : Target.Transfo.local =
  Target.apply_on_path (insert_aux name td_body index)
