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
    | Trm_let (vk, (x, _), dx) ->
        let t_x =
          if as_reference then trm_apps (trm_unop Unop_get) [trm_var x]
          else trm_var x
        in
        let def_x =
          if not as_reference then 
            begin match vk with 
            | Var_immutable -> dx
            | _ -> begin match dx.desc with 
                   | Trm_apps(_, [init]) -> init
                   | _ -> fail t.loc "fold_aux: expected a new operation"
                   end
            end
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

     | _ -> fail t.loc "fold_decl: expected a variable declaration"
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
let insert_aux (ctx : Trace.context) (const : bool) (as_reference : bool) (x : var) (dx : string) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let context = Generic_core.get_context ctx t in
    let dx = Generic_core.term ~context ctx dx in
    let tx = match dx.typ with
    | None -> fail t.loc "insert_aux: cannot find definition type"
    | Some tx -> if as_reference then typ_ptr Ptr_kind_mut tx else tx
    in
    let def_x =
      if as_reference then {dx with add = Add_address_of_operator :: dx .add}
      else dx
    in
    let t_insert =
      if const then trm_let Var_immutable (x,tx) def_x
      else
        trm_let Var_mutable (x, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut tx) (trm_apps (trm_prim (Prim_new tx)) [def_x])

    in
    let tl = Tools.list_insert (index) t_insert tl in
    trm_seq ~annot:t.annot tl
  | _ -> fail t.loc "insert_aux: expected the surrounding sequence"

(* [insert const as_reference x dx index p t] *)
let insert (ctx : Trace.context )(const : bool) (as_reference : bool) (x : var) (dx : string) (index : int) : Target.Transfo.local =
  Target.apply_on_path (insert_aux ctx const as_reference x dx index)


let insert_and_fold_aux (ctx : Trace.context) (const : bool) (as_reference : bool) (x : var) (dx : string) (index : int) (fold_at : target list) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let context = Generic_core.get_context ctx (trm_seq ~annot:(Some No_braces) lfront) in
    let dx = Generic_core.term ~context ctx dx in
    let tx = match dx.typ with
    | None -> fail t.loc "insert_and_fold_aux: cannot find definition type"
    | Some tx -> if as_reference then typ_ptr Ptr_kind_mut tx else tx
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
  let insert_and_fold (ctx : Trace.context) (const : bool) (as_reference : bool) (x : var) (dx : string) (index : int) (fold_at : target list) : Target.Transfo.local =
    Target.apply_on_path(insert_and_fold_aux ctx const as_reference x dx index fold_at)

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
    | Trm_let (vk, (x, _), dx) ->
      let t_x = 
      begin match vk with 
      | Var_immutable -> trm_var x
      | _ -> trm_apps ~annot:(Some Mutable_var_get) (trm_unop Unop_get) [trm_var x] 
      end
       in
      let def_x = 
      begin match vk with 
            | Var_immutable -> dx
            | _ -> begin match dx.desc with 
                   | Trm_apps(_, [init]) -> init
                   | _ -> fail t.loc "inline_aux: expected a new operation"
                   end
      end 
       in
      let lback = List.map (Generic_core.change_trm ~change_at:inline_at t_x def_x) lback in
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


