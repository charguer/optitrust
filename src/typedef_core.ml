open Ast
open Target

(* [fold_aux as_reference fold_at]: This is an auxiliary function for fold
    params:
      fold_at: targets where folding should be performed, if left empty then folding is applied everywhere
      t: ast subterm
    return:
      the update ast
*)
let fold_aux (fold_at : target list) (index : int) (t : trm) : trm=
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
        let lback = List.map (Internal.change_typ ~change_at:fold_at dx ty_x) lback in
        trm_seq ~annot:t.annot (lfront @ [d] @ lback)
       | _ -> fail t.loc "fold_decl: expected a typedef"
       end
     | _ -> fail t.loc "fold_decl: expected a type definition"
     end


  | _ -> fail t.loc "fold_aux: expected the surrounding sequence"

(* [fold fold_at index p t] *)
let fold (fold_at : target list) (index) : Target.Transfo.local =
  Target.apply_on_path(fold_aux fold_at index)


(* [insert_aux x dx t]: This function is an auxiliary function for insert_typedef
      params:
        x: typvar representing the type variable for the new typedef
        dx: value of the typedef
        index: where the new typedef is going to be inserted
        t: ast subterm
*)
let insert_aux (ctx : Trace.context) (s : string) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let context = Internal.get_context ctx t in
    let t_insert = Internal.term ~context ctx s in
    let tl = Tools.list_insert (index-1) t_insert tl in
    trm_seq ~annot:t.annot tl
  | _ -> fail t.loc "insert_aux: expected the surrounding sequence"

(* [insert_typedef x dx index p t] *)
let insert (ctx : Trace.context) (s : string) (index : int) : Target.Transfo.local =
  Target.apply_on_path (insert_aux ctx s index)


(* [insert_and_fold_aux const as_reference fold_at x dx index t]: This is an auxiliary function for insert_and_fold_typedef
    params:
      x: name of the variable
      dx: value of the typedef
      index: the index where we want to insert the declaration
      t: ast subterm
    return:
      the updated ast
*)
let insert_and_fold_aux (ctx : Trace.context) (td : string) (index : int) (fold_at : target list) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let context = Internal.get_context ctx t in
    let dx = Internal.term ~context ctx td in
    begin match dx.desc with 
    | Trm_typedef td ->
      begin match td.typdef_body with
      | Typdef_alias typ ->
          let ty_x = typ_constr td.typdef_tconstr td.typdef_typid []  in
          let lback = List.map(Internal.change_typ ~change_at:fold_at typ ty_x) lback in
          trm_seq (lfront @ [dx] @ lback)
      | _ -> fail dx.loc "insert_and_fold_aux: this feature is supported only for type aliases"
      end
    | _ -> fail t.loc "insert_and_fold_aux: the inserted trm should be a typedef"
    end
    
  | _ -> fail t.loc "insert_and_fold_aux: expected the surrounding sequence"


  (* [insert_and_fold x dx index fodl_at] *)
  let insert_and_fold (ctx : Trace.context) (td : string) (index : int) (fold_at : target list) : Target.Transfo.local =
    Target.apply_on_path(insert_and_fold_aux ctx td index fold_at)


(* [inline_aux inline_at]: This is an auxiliary function for inline
    params:
      delete_decl: delete or don't delete the declaration of the variable
      inline_at: targets where inlining should be performed, if empty inlining is applied everywhere
      t: ast subterm
    return:
      the updated ast
*)
let inline_aux (delete_decl : bool) (inline_at : target list) (index : int) (t : trm) : trm =
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
      let lback = List.map(Internal.change_typ ~change_at:inline_at ty_x dx) lback in
      let tl =
        if delete_decl then lfront @ lback
        else lfront @ [dl] @ lback
      in
      trm_seq ~annot:t.annot tl
     | _ -> fail t.loc "inline_aux: expected a typdef_alias"
     end
    | _ -> fail t.loc "inline_aux: expected a typedef declaration"
    end
  | _ -> fail t.loc "inline_aux: expected the surrounding sequence"

(* [inline delete_decl inline_at index t p] *)
let inline (delete_decl : bool) (inline_at : target list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (inline_aux delete_decl inline_at index)

