open Prelude
open Target

(** [fold_at fold_at index]: replaces occurrences of the typedef underlying type with the defined type,
       [fold_at] - targets where folding should be performed, if left empty then folding is applied recursively
                 on all the trms that belong to the same sequence as typedef,
       [index] - index of the typedef on its surrounding sequence,
       [t] - ast of the sequence that contains the targeted typedef. *)
let fold_at (fold_at : target) (index : int) (t : trm) : trm=
  let error = "Typedef_core.fold_at: expected the surrounding sequence." in
  let tl = trm_inv ~error trm_seq_inv t in
  let f_update (t : trm) : trm = t in
  let f_update_further (t : trm) : trm =
    let d = Mlist.nth tl index in
    let dx, ty_x =
    begin match d.desc with
    | Trm_typedef td ->
      begin match td.typedef_body with
      | Typedef_alias dx ->
         dx, typ_var td.typedef_name
      | _ -> trm_fail d "Typedef_core.fold_at: expected a type definition"
      end
    | _ -> trm_fail d "Typedef_core.fold_at: expected a typedef"
    end in

  Internal.change_typ ~change_at:[fold_at] dx ty_x t in
  let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
  trm_seq ~annot:t.annot new_tl

(** [unfold_at unfold_at]: replaces occurrences of the defined type with its underlying type,
      [delete] - a flag for deciding if we should delete or not the typedef declaration,
      [unfold_at] - targets where unfolding should be performed, if empty unfolding is applied
                   on all the trms that are with in the same level as the targeted typedef,
      [t] - ast of the sequence that contains the targeted typedef. *)
let unfold_at (delete : bool) (unfold_at : target) (index : int) (t : trm) : trm =
  let error = "Typedef_core.unfold_at: expected the surrounding sequence." in
  let tl = trm_inv ~error trm_seq_inv t in
  let f_update (t : trm) : trm = t in
  let f_update_further (t : trm) : trm =
    let d = Mlist.nth tl index in
    let dx, ty_x =
    begin match d.desc with
    | Trm_typedef td ->
      begin match td.typedef_body with
      | Typedef_alias dx ->
        let ty_x = typ_var td.typedef_name in
        dx, ty_x
      | _ -> trm_fail d "Typedef_core.unfold_at: expected a typedef alias"
      end
    | _ -> trm_fail d "Typedef_core.unfold_at: expected a typdef_alias"
    end in Internal.change_typ ~change_at:[unfold_at] ty_x dx t in
  let new_tl = Mlist.update_at_index_and_fix_beyond ~delete index f_update f_update_further tl in
  trm_seq ~annot:t.annot new_tl

(** [insert_copy_of name index t]: creates a copy of the targeted typedef
      [name] - new typ name
      [t] - ast of the surrounding sequence of the targeted typedef *)
let insert_copy_of (name : string) (t : trm) : trm =
  let error = "Typedef_core.insert_copy_of: expected a typedef declaration." in
  let td = trm_inv ~error trm_typedef_inv t in
  let td_copy = trm_typedef {td with typedef_name = name_to_typvar name} in
  trm_seq_nobrace_nomarks [t; td_copy]

(** [insert_at name td_body index]: inserts a new type definition,
      [name] - new type name,
      [td_body] - body of the new type definition,
      [index] - location where the typedef should be inserted inside a sequence. *)
let insert_at (name : string) (td_body : typedef_body) (index : int) (t : trm) : trm =
  let error = "Typedef_core.insert_at: expected the surrounding sequence." in
  let tl = trm_inv ~error trm_seq_inv t in
  let trm_to_insert = trm_typedef {typedef_name = name_to_typvar name; typedef_body = td_body } in
  let new_tl = Mlist.insert_at index trm_to_insert tl in
  trm_seq ~annot:t.annot new_tl
