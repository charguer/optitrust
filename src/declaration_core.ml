open Ast
open Target

(* [fold_aux as_reference fold_at]: This is an auxiliary function for fold
    params:
      as_reference: check if the variable inside the declaration is heap allocated or not
      fold_at: targets where folding should be performed, if left empty then folding is applied everywhere
      t: ast subterm
    return:
      the update ast
*)
let fold_aux (as_reference : bool) (fold_at : target list) (index : int) (t : trm) : trm=
  match t.desc with
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let d, lback = Tools.split_list_at 1 lback in
    let d = List.hd d in
    begin match d.desc with
    | Trm_let (_,(x,_),dx) ->
        let t_x =
          if as_reference then trm_apps (trm_unop Unop_get) [trm_var x]
          else trm_var x
        in
        let def_x =
          if not as_reference then dx
          else
            match dx.add with
            | Add_address_of_operator :: addl -> {dx with add = addl}
            | _ -> fail d.loc "fold_decl: expected a reference"
        in
        let lback = List.map(Generic_core.change_trm ~change_at:fold_at def_x t_x) lback
        (*
          def_x might have been replaced with x in the definition of x
          -> replace it again with def_x
         *)
        in

        trm_seq (lfront @ [d] @ lback)

     (* typedef *)
     | Trm_typedef td ->
       begin match td.typdef_body with
       | Typdef_alias dx ->
        let ty_x = typ_var td.typdef_tconstr in
        let lback = List.map(Generic_core.change_typ ~change_at:fold_at dx ty_x) lback in
        let change_at = [[cTypDef x]] in
        let lback = List.map(Generic_core.change_typ ~change_at ty_x dx) lback in
        trm_seq (lfront @ [d] @ lback)
       | _ -> fail t.loc "fold_decl: expected a typedef"
       end
     (* fun decl *)
     | Trm_let_fun _ ->
        fail t.loc "fold_decl: fun declaration folding is unsupported"
     | _ -> fail t.loc "fold_decl: expected a definition"
     end


  | _ -> fail t.loc "fold_aux: expected the surrounding sequence"

(* [fodl as_reference fold_at index p t] *)
let fold (as_reference : bool) (fold_at : target list) (index) : Target.Transfo.local =
  Target.apply_on_path(fold_aux as_reference fold_at index)

(* [insert_aux const as_reference x dx t]: This is an auxiliary function for insert
    params:
      const: boolean for the mutability of the declaration
      as_reference: boolean about the type of allocation
      x: name of the variable
      dx: value of the variable
      index: the index where we want to insert the declaration
    return:
      the updated ast
*)
let insert_aux (const : bool) (as_reference : bool) (x : var) (dx : string) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let dx = Generic_core.term dx in
    let tx = match dx.typ with
    | None -> fail t.loc "insert_aux: cannot find definition type"
    | Some tx -> if as_reference then typ_ptr tx else tx
    in
    let def_x =
      if as_reference then {dx with add = Add_address_of_operator :: dx .add}
      else dx
    in
    let t_insert =
      if const then trm_let Var_immutable (x,tx) def_x
      else
        trm_let Var_mutable (x, tx) (trm_apps (trm_prim (Prim_new tx)) [def_x])

    in
    let tl = Tools.list_insert (index) t_insert tl in
    trm_seq ~annot:t.annot tl
  | _ -> fail t.loc "insert_aux: expected the surrounding sequence"

(* [insert const as_reference x dx index p t] *)
let insert(const : bool) (as_reference : bool) (x : var) (dx : string) (index : int) : Target.Transfo.local =
  Target.apply_on_path (insert_aux const as_reference x dx index)

(* [insert_typedef_aux x dx t]: This function is an auxiliary function for insert_typedef
      params:
        x: typvar representing the type variable for the new typedef
        dx: value of the typedef
        index: where the new typedef is going to be inserted
        t: ast subterm
*)
let insert_typedef_aux (x : typvar) (dx : typ) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let td = trm_typedef {
        typdef_typid = 0;
        typdef_tconstr = x;
        typdef_vars = [];
        typdef_body = Typdef_alias dx}
      in
    let t_insert = td 
      in
    let tl = Tools.list_insert (index) t_insert tl in
    trm_seq ~annot:t.annot tl
  | _ -> fail t.loc "insert_typedef_aux: expected the surrounding sequence"

(* [insert_typedef x dx index p t] *)
let insert_typedef (x : typvar) (dx : typ) (index : int) : Target.Transfo.local =
  Target.apply_on_path (insert_typedef_aux x dx index)

(* [insert_and_fold_aux const as_reference fold_at x dx index t]: This is an auxiliary function for insert_and_fold
    params:
      const: boolean for the mutability of the declaration
      as_reference: boolean about the type of allocation
      x: name of the variable
      dx: value of the variable
      index: the index where we want to insert the declaration

    return:
      the updated ast
*)
let insert_and_fold_aux (const : bool) (as_reference : bool) (x : var) (dx : trm) (index : int) (fold_at : target list) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let tx = match dx.typ with
    | None -> fail t.loc "insert_and_fold_aux: cannot find definition type"
    | Some tx -> if as_reference then typ_ptr tx else tx
    in
    let def_x =
      if as_reference then {dx with add = Add_address_of_operator :: dx.add}
      else dx
    in
    let t_insert =
      if const  then trm_let Var_immutable (x,tx) def_x
      else
        trm_let Var_mutable (x, tx) (trm_apps (trm_prim (Prim_new tx)) [def_x])
    in
    let t_x =
      if as_reference then trm_apps (trm_unop Unop_get) [trm_var x]
      else trm_var x
    in
    let def_1_x =
      if not as_reference then dx
      else
        match dx.add with
        | Add_address_of_operator :: addl -> {dx with add = addl}
        | _ -> fail t.loc "insert_and_fold_aux: expected a reference"
    in
    let lback = List.map (Generic_core.change_trm ~change_at:fold_at def_1_x t_x) lback in
    trm_seq (lfront @ [t_insert] @ lback)

  | _ -> fail t.loc "insert_and_fold_aux: expected the surrounding sequence"


  (* [insert_and_fold const as_reference x dx index fodl_at] *)
  let insert_and_fold (const : bool) (as_reference : bool) (x : var) (dx : trm) (index : int) (fold_at : target list) : Target.Transfo.local =
    Target.apply_on_path(insert_and_fold_aux const as_reference x dx index fold_at)


  (* [insert_and_fold_typedef_aux const as_reference fold_at x dx index t]: This is an auxiliary function for insert_and_fold_typedef
    params:
      x: name of the variable
      dx: value of the typedef
      index: the index where we want to insert the declaration
      t: ast subterm
    return:
      the updated ast
*)
let insert_and_fold_typedef_aux (x : var) (dx : typ) (index : int) (fold_at : target list) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let tid = Tools.next_typid()
    let t_insert = trm_typedef {
      typdef_typid = tid;
      typdef_tconstr = x;
      typdef_vars = [];
      typdef_body = Typdef_alias dx}
      in
    let ty_x = typ_constr x tid []  in
    let lback = List.map(Generic_core.change_typ ~change_at:fold_at dx ty_x) lback in
    trm_seq (lfront @ [t_insert] @ lback)
  | _ -> fail t.loc "insert_and_fold_aux: expected the surrounding sequence"


  (* [insert_and_fold x dx index fodl_at] *)
  let insert_and_fold_typedef (x : var) (dx : typ) (index : int) (fold_at : target list) : Target.Transfo.local =
    Target.apply_on_path(insert_and_fold_typedef_aux x dx index fold_at)


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
    let dl, lback = Tools.split_list_at 0 lback in
    let dl = List.hd dl in
    begin match dl.desc with
    | Trm_let (_,(x,_), dx) ->
      let t_x = trm_apps ~annot:(Some Mutable_var_get) (trm_unop Unop_get) [trm_var x] in
      let lback = List.map (Generic_core.change_trm ~change_at:inline_at t_x dx) lback in
      let tl =
        if delete_decl then lfront @ lback
        else lfront @ [dl] @ lback
      in
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "inline_aux: expected a variable declaration"
    end
  | _ -> fail t.loc "inline_aux: expected the surrounding sequence"


(* [inline delete_decl inline_at index t p] *)
let inline (delete_decl : bool) (inline_at : target list) (index : int) : Target.Transfo.local =
  Target.apply_on_path(inline_aux delete_decl inline_at index)

(* [inline_typedef_aux inline_at]: This is an auxiliary function for inline_typedef
    params:
      delete_decl: delete or don't delete the declaration of the variable
      inline_at: targets where inlining should be performed, if empty inlining is applied everywhere
      t: ast subterm
    return:
      the updated ast
*)
let inline_typedef_aux (delete_decl : bool) (inline_at : target list) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let dl, lback = Tools.split_list_at 1 lback in
    let dl = List.hd dl in
    begin match dl.desc with
    | Trm_typedef td ->
     begin match td.typdef_body with
     | Typedef_alias dx ->
      let ty_x = typ_var td.typdef_tconstr in
      let lback = List.map(Generic_core.change_typ ~change_at:inline_at ty_x dx) lback in
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
let inline_typedef (delete_decl : bool) (inline_at : target list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (inline_typedef_aux delete_decl inline_at index)

(*


*)

