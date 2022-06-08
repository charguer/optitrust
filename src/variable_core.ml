open Ast
open Target

(* [fold_aux as_reference fold_at index t]: fold the targeted variable declaration,
      [fold_at] - target where folding should be performed, if left empty
                  then folding is applied everywhere,
      [index] - the index of the targeted declaration on its surroudinig sequence,
      [t] - ast of the sequence that contains the targeted declaration. *)
let fold_aux (fold_at : target) (index : int) (t : trm) : trm=
  let error = "Variable_core.fold_aux: expected the surrounding sequence." in
  let tl = trm_inv ~error trm_seq_inv t in
  let f_update (t1 : trm) : trm = t1 in
  let f_update_further (t1 : trm) : trm =
    let t_dl = Mlist.nth tl index in
    match t_dl.desc with
    | Trm_let (vk, (x, tx), dx) ->
      (* check if the declaration is of the form int*x = &y *)
      let as_reference = is_typ_ptr (get_inner_ptr_type tx) && not (trm_has_cstyle Reference t_dl) in
      let t_x =
        if as_reference then trm_var_get x
        else if trm_has_cstyle Stackvar t_dl then trm_var_get x
        else trm_var x
      in
      let def_x =
          begin match vk with
          | Var_immutable -> dx
          | _ -> begin match dx.desc with
                 | Trm_apps(_, [init]) -> init
                 | _ -> dx
                 end
          end in
      Internal.change_trm ~change_at:[fold_at] def_x t_x t1
    | _ -> fail t_dl.loc "Variable_core.fold_decl: expected a variable declaration"
     in
    let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
    trm_seq ~annot:t.annot new_tl

(* [fold fold_at index t p]: applies [fold_aux] at trm [t] with path [p]. *)
let fold (fold_at : target) (index) : Transfo.local =
  apply_on_path(fold_aux fold_at index)


(* [unfold_aux delete_decl accept_functions mark unfold_at index t]: unfolds the targeted declaration,
      [delete_decl] - if true deletes the targeted declaration,
      [accept_functions] - if true unfolds functions too,
      [mark] - add a mark at the intialization trm of the declaratin,
      [unfold_at] - target where unfolding should be performed, if empty all the occurrences
                   of the variable are replaced with its initialization value,
      [index] - index of the targeted declaration inside its surrounding sequence,
      [t] - ast of the sequence that contains the targeted declaration.*)
let unfold_aux (delete_decl : bool) (accept_functions : bool) (mark : mark) (unfold_at : target) (index : int) (t : trm) : trm =
  let error = "Variable_core.unfodl_aux: expected the surrounding sequence." in
  let tl = trm_inv ~error trm_seq_inv t in
  let f_update (t : trm) : trm = t in
  let f_update_further (t : trm) : trm =
    let dl = Mlist.nth tl index in
    match dl.desc with
    | Trm_let (vk, (x, _), init) ->
      let init = trm_add_mark mark init in
      begin match vk with
      | Var_immutable ->
        begin match unfold_at with
        | [] -> Internal.subst_var x init t
        | _ -> Internal.change_trm ~change_at:[unfold_at] (trm_var x) init t
        end
      | Var_mutable ->
        if trm_has_cstyle Reference dl
          then
            begin match unfold_at with
            | [] -> Internal.subst_var x init t
            | _ -> Internal.change_trm ~change_at:[unfold_at] (trm_var x) init t
            end
          else fail dl.loc "Variable_core.unfold_aux: only const variables are safe to unfold"
      end
    (* qvar_inv_var (qx : qvar) : var option =
         if qx.qvar_path = [] then Some qx.qvar_var
           else None
        M :: f(int x)

        Target: 
          cFunDef "f" shouldn't match

          cFunDef "M :: f" 
          cFunDef ~qpath:["M"] "f"
          cFunDef ~qvar:(qvar ["M"] "f") ""


    
     *)
    | Trm_let_fun (f, _, _, _) ->
      if accept_functions
        then Internal.subst_var f dl t
        else fail dl.loc "Varialbe_core.unfold_aux: to replace function calls with their declaration you need to set accept_functions arg to true"
    | _ -> fail t.loc "Variable_core.unfodl_aux: expected a target to a variable or function definition"
  in
  let new_tl = Mlist.update_at_index_and_fix_beyond ~delete:delete_decl index f_update f_update_further tl in
  trm_seq ~annot:t.annot new_tl

(* [unfold delete_decl accept_functions mark unfold_at index t p]: applies [unfold_aux] at trm [t] with path [p]. *)
let unfold (delete_decl : bool) (accept_functions : bool) (mark : mark) (unfold_at : target) (index : int) : Transfo.local =
  apply_on_path(unfold_aux delete_decl accept_functions mark unfold_at index)


(* [rename_aux index new_name t]: renames the variable declared on the targeted declaration all its occurrences,
      [index] - index of the targeted declaration inside its surrounding sequence,
      [new_name] - the new name for the targeted variable,
      [t] - ast of the sequence that contains the targeted declaration. *)
let rename_aux (index : int) (new_name : var) (t : trm) : trm =
  let error = "Variable_core.rename_aux: expected the surrounding sequence of the targeted declaration." in
  let tl = trm_inv ~error trm_seq_inv t in
  let f_update (t : trm) : trm =
    match t.desc with
    | Trm_let (vk,( x, tx), init) ->
      trm_let ~annot:t.annot vk (new_name, tx) init
    | _ -> fail t.loc "Variable_core.rename_aux: expected a target to variable declaration"
    in
  let f_update_further (t : trm) : trm =
    let dl = Mlist.nth tl index in
    let x = begin match decl_name dl with
    | Some x -> x
    | None -> fail t.loc "Variable_core.rename_aux: expected a target to a variable declaration"
    end in
    let rec aux (t1 : trm) : trm =
      match t1.desc with
      | Trm_var (vk, y) when y = x -> trm_replace (Trm_var (vk, new_name)) t
      | _ -> trm_map aux t1
    in aux t
  in
  let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
  trm_seq ~annot:t.annot new_tl

(* 

  instead of when y = x
  when qvar_eq y x 
    ignores the string repr in the comparison
    



    M :: f(int x)

    rename ~new_name:"N :: g" [cFunDef "M :: f"];



 *)



(* [rename new_name index t p]: applies [rename_aux] at trm [t] with path [p]. *)
let rename (new_name : var) (index : int): Transfo.local =
  apply_on_path (rename_aux index new_name)

(* [subst_aux name space t]: replaces all occurrences of [name] with [space],
        [name] - name of the variable whose occurrences are going to be replaced,
        [space] - trm which is going to replace all the occurrences of [name],
        [t] - any node in the ast that contains an occurrence of [name]. *)
let subst_aux (name : var) (space : trm) (t : trm) : trm =
  Internal.subst_var name space t

(* [subst name space t p]: applies [subst_aux] at trm [t] with path [p] *)
let subst (name : var)(space : trm) : Transfo.local =
  apply_on_path (subst_aux name space)



(* [init_detach_aux t]: detaches the targeted variable declaration,
      [t] - ast of the targeted variable declaration. *)
let init_detach_aux  (t : trm) : trm =
  let error = "Variable_core.init_detach_aux: variable could not be matched, make sure your path is correct." in
  let (vk,x, tx, init) = trm_inv ~error trm_let_inv t in
  begin match vk with
  | Var_immutable -> fail t.loc "init_detach_aux: const declarations cannot be detached"
  | _ ->
    let init =
      begin match init.desc with
      | Trm_apps(_,[init]) -> init
      | _ -> fail t.loc "init_detach_aux: can't detach an uninitialized declaration"
      end in
    let var_type = get_inner_ptr_type tx in
    let var_decl = trm_pass_marks t (trm_let_mut ~annot:t.annot (x, var_type) (trm_uninitialized ())) in
    (* Check if variable was declared as a reference *)
    let var_assgn = trm_set (trm_var ~typ:(Some var_type) x) {init with typ = (Some var_type)} in
    trm_seq_no_brace [var_decl; var_assgn]
  end

(* [init_detach t p]: applies [init_detach_aux] at trm [t] with path [p]. *)
let init_detach : Transfo.local =
  apply_on_path(init_detach_aux )

(* [Init_attach_no_occurrences]: raised by [init_attach_aux]. *)
exception Init_attach_no_occurrences

(* [Init_attach_occurrence_below_control]: raised by [init_attach_aux]. *)
exception Init_attach_occurrence_below_control


(* [init_attach_aux t]: attaches a variable declaration to its unique write operation,
      [const] - a boolean to decide if the attached variable should be mutable or not,
      [index] - index of the targeted instruction inside its surrounding sequence,
      [t] - ast of the surrounding sequence of the variable declaration.

    NOTE: if no set operation on the targeted variable was found then Init_attach_no_occurrences is raised
          if more then one set operation on the targeted variable was found then Init_attach_occurrence_below_control is raised *)
let init_attach_aux (const : bool) (index : int) (t : trm) : trm =
  let error = "Variable_core.init_attach_axu: expected the surrounding sequence." in
  let tl = trm_inv ~error trm_seq_inv t in
    let lfront, trm_to_change, lback  = Internal.get_item_and_its_relatives index tl in
    let error = "Variable_core.init_attach_aux: expected a variable declaration." in
    let (_, x, tx, _) = trm_inv ~error trm_let_inv trm_to_change in
    let tg = [nbAny;cSeq (); cStrict;cWriteVar x] in
    let new_tl = Mlist.merge lfront lback in
    let new_t = trm_seq ~annot:t.annot new_tl in
    let ps = resolve_target tg new_t in
    let nb_occs = List.length ps in
    if nb_occs = 0 then raise Init_attach_no_occurrences
     else if nb_occs >= 2 then raise Init_attach_occurrence_below_control;
    Xlist.fold_lefti (fun i acc p ->
      if i = 0 then begin
      apply_on_path (fun t1 ->
        begin match t1.desc with
        | Trm_apps (_, [_;rs]) ->
          let decl = if const then trm_let_immut (x, tx) rs else trm_let_mut (x, get_inner_ptr_type tx) rs in
          trm_pass_marks trm_to_change decl
        | _ -> t1
        end
      ) acc p
      end
      else acc
    ) new_t ps

(* [init_attach const index t p]: applies [init_attach_aux] at trm [t] with path [p]. *)
let init_attach (const : bool) (index : int) : Transfo.local =
  apply_on_path(init_attach_aux const index )

(* [local_name_aux var_type curr_var local_var t]: adds a local variable declaration and
      replace all the occurrences of [curr_var] with [local_var] in [t],
      [mark] - a mark to mark the producesd nobrace sequence,
      [curr_var] - the previous name of the variable, this is used to find all its occurrences,
      [local_var] - the name of the variable to be declared and replace all the occurrences of [curr_var],
      [t] - ast of the trm that contains [curr_var]. *)
let local_name_aux (mark : mark) (curr_var : var) (local_var : var) (t : trm) : trm =
  let vardef_trm = begin match get_trm_at [cVarDef curr_var] with
    | Some vt -> vt
    | None -> fail None "local_name_aux: couldn't find the variable provided as argument"
    end in
  let var_type = match trm_var_def_inv vardef_trm with
    | Some (_, _, ty, _) -> ty
    | _ -> fail vardef_trm.loc "Variable_core.local_name: make sure the name of the current var is entered correctly"
    in
  let fst_instr = trm_let_mut (local_var, var_type) (trm_var_possibly_mut ~typ:(Some var_type) curr_var) in
  let lst_instr = trm_set (trm_var ~typ:(Some var_type) curr_var) (trm_var_possibly_mut ~typ:(Some var_type) local_var) in
  let new_t = Internal.change_trm (trm_var curr_var) (trm_var local_var) t in
  let final_trm = trm_seq_no_brace [fst_instr;new_t;lst_instr] in
  trm_add_mark mark final_trm

(* [local_name mark curr_var local_var t p]: applies [local_name_aux] at trm [t] with path [p]. *)
let local_name (mark : mark) (curr_var : var) (local_var : var) : Transfo.local =
  apply_on_path(local_name_aux mark curr_var local_var)

(* [delocalize_aux array_size ops index t]: see [Variable_basic.delocalize],
      [array_size] - size of the arrays to be declared inside the targeted sequence,
      [ops] - delocalize operation representing the unitary lement used for initialization
             and the fold_lefting operation used for the reduction,
      [index] - the index for the two added loops,
      [t] - the ast of the sequence generated after applying the local_name transformation. *)
let delocalize_aux (array_size : string) (ops : local_ops) (index : string) (t : trm) : trm =
  let error = "Variable_core.delocalize_aux: expected the nobrace sequence." in
  let tl = trm_inv ~error trm_seq_inv t in
    if Mlist.length tl <> 3 then fail t.loc "delocalize_aux: the targeted sequence does not have the correct shape";
    let def = Mlist.nth tl 0 in
    let snd_instr = Mlist.nth tl 1 in
    begin match def.desc with
    | Trm_let (vk, (x, tx), init) ->
      let local_var = x in
      let curr_var_trm = match get_init_val init with
        | Some init1 -> init1
        | _ -> fail def.loc "delocalize_aux: couldn't get the value of the current variable " in
      let curr_var_trm = get_operation_arg curr_var_trm in
      let var_type = (get_inner_ptr_type tx) in
      let init_trm, op = begin match ops with
        | Local_arith (li, op) ->
            trm_lit li, (trm_prim_compound op
                               curr_var_trm
                                (trm_get (trm_apps (trm_binop Binop_array_access)[trm_var_get local_var; trm_var index])))
        | Local_obj (clear_f, transfer_f, _) ->
            trm_apps ~typ:(Some (typ_unit ())) (trm_var clear_f) [],
            trm_apps ~typ:(Some (typ_unit())) (trm_var transfer_f)
              [trm_get curr_var_trm ;
              trm_get (trm_apps (trm_binop Binop_array_access)[trm_var_get local_var; trm_var index])]
      end in
      let new_first_trm = trm_seq_no_brace[
          trm_let_array vk (local_var, var_type) (Trm (trm_var array_size)) (trm_uninitialized ());
          trm_set (trm_apps (trm_binop Binop_array_access)[trm_var_get local_var; trm_lit (Lit_int 0)]) (trm_get curr_var_trm);
          trm_for (index, (trm_int 1), DirUp, (trm_var array_size), Post_inc, false)
         (trm_seq_nomarks [trm_set (trm_apps (trm_binop Binop_array_access)[trm_var_get local_var; trm_var index]) init_trm])]
          in
      let new_snd_instr = Internal.subst_var local_var  (trm_apps (trm_binop Binop_array_access)[trm_var_get local_var; trm_apps (trm_var "ANY") [trm_var array_size] ]) snd_instr  in
      let new_thrd_trm = trm_seq_no_brace [
                      trm_set (curr_var_trm) (trm_get (trm_apps (trm_binop Binop_array_access)[trm_var_get local_var; trm_lit (Lit_int 0)]));
                      trm_for (index, (trm_int 1), DirUp, (trm_var array_size), Post_inc, false) (trm_seq_nomarks [op])
                     ] in
      let new_tl = (Mlist.of_list [new_first_trm; new_snd_instr; new_thrd_trm]) in
      { t with desc = Trm_seq new_tl}
      (* trm_seq ~annot:t.annot (Mlist.of_list [new_first_trm; new_snd_instr; new_thrd_trm]) *)

    | _ -> fail t.loc "Variable_core.delocalize_aux: first instruction in the sequence should be the declaration of local variable"
    end


(* [delocalize array_size ops index t p]: applies [delocalize_aux] at trm [t] with path [p]. *)
let delocalize (array_size : string) (ops : local_ops) (index : string) : Transfo.local =
  apply_on_path (delocalize_aux array_size ops index )



(* [insert_aux index const name typ value t]: inserts a variable declaration on sequence [t],
      [index] - location where the declaration is going to be inserted,
      [const] - a flag on the mutability of the variable [name],
      [name] - name of the inserted variable,
      [typ] - the type of the inserted variable,
      [value] - the initial value of the inserted variable [name] entered as a string,
      [t] - ast of the sequence where the insertion is performed. *)
let insert_aux (index : int) (const : bool) (name : string) (typ : typ) (value : trm) (t : trm) : trm =
  let error = "Variable_core.insert_aux: expected the sequence where the declaration is oing to be inserted" in 
  let tl = trm_inv ~error trm_seq_inv t in
  let new_decl = if const then trm_let_immut (name, typ) value else trm_let_mut (name, typ) value in
  let new_tl = Mlist.insert_at index new_decl tl in
  trm_seq ~annot:t.annot new_tl

(* [insert index const name typ value t p]: applies [insert_aux] at trm [t] with path [p]. *)
let insert (index : int) (const : bool) (name : string) (typ : typ) (value : trm) : Transfo.local =
  apply_on_path (insert_aux index const name typ value)


(* [change_type_aux new_type t]: changes the current type of the targeted variable,
      [new_type] - the new type replacing the current one entered as a string,
      [t] - ast of the sequence that contains the targeted declaration. *)
let change_type_aux (new_type : typvar) (index : int) (t : trm) : trm =
  let new_type = ty new_type in
  match t.desc with
  | Trm_seq tl ->
    let f_update (t : trm) : trm =
      let error = "Variable_core.change_type_aux: expected a target to a variable declaration." in
      let (vk, x, tx, init) = trm_inv ~error trm_let_inv t in
        Internal.change_typ (get_inner_ptr_type tx) new_type t
     in
    let f_update_further (t : trm) : trm =
      let dl = Mlist.nth tl index in
      let error = "Variable_core.change_type_aux: expected a target to a variable declaration." in
      let (_, x, tx, _) = trm_inv ~error trm_let_inv dl in
        Internal.change_typ (get_inner_ptr_type tx) new_type ~change_at:[[cVar x]] t
      in
    let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Variable_core.change_type_aux: expected the surrounding sequence"

(* [change_type new_type index t p]: applies [change_type_aux] at trm [t] with path [p]. *)
let change_type (new_type : typvar) (index : int) : Transfo.local =
  apply_on_path (change_type_aux new_type index)

(* [bind_aux index fresh_name const p_local t]: binds the variable [fresh_name] to the targeted trm,
      [my_mark] - a mark to be left in the targeted trm,
      [index] - index of the instruction containing the targeted function call,
      [fresh_name] - name of the variable which going to be binded to the function call,
      [const] - a flag for the mutability of the binded variable,
      [p_local] - the local path from the instruction containing the targeted node
                  to the targeted node,
      [t] - ast of the sequence containing the targeted node. *)
let bind_aux (my_mark : mark) (index : int) (fresh_name : var) (const : bool) (is_ptr : bool) (typ : typ option) (p_local : path) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let f_update (t : trm) : trm =
      let targeted_node = Path.resolve_path p_local t in
      let has_reference_type = if (Str.string_before fresh_name 1) = "&" then true else false in
      let fresh_name = if has_reference_type then (Str.string_after fresh_name 1) else fresh_name in
      let node_type = match targeted_node.typ with
      | Some ty -> ty
      | _ -> typ_auto()
       in
      let node_to_change = Internal.change_trm targeted_node (trm_var_possibly_mut ~const ~typ:(Some node_type) fresh_name) t in
      let targeted_node = trm_add_mark my_mark targeted_node in
      let decl_to_insert =
      begin match targeted_node.desc with
      | Trm_array tl ->
        let node_type = begin match node_type.typ_desc with
        | Typ_array (ty, _) -> get_inner_const_type ty
        | _ -> typ_auto ()
        end in
        let sz = (Mlist.length tl)  in
        if const
          then
            trm_let_array Var_immutable (fresh_name, node_type) (Const sz) targeted_node
          else
            trm_let_array Var_mutable (fresh_name, node_type) (Const sz) targeted_node
      | _ ->
        let node_type = if is_ptr then typ_ptr Ptr_kind_mut node_type else node_type in
        let node_type = begin match typ with | Some ty -> ty | _ -> node_type end in
        if const
          then trm_let_immut (fresh_name, node_type) targeted_node
          else trm_let_mut (fresh_name, node_type) targeted_node
      end in
      trm_seq_no_brace [decl_to_insert; node_to_change]
    in
    let new_tl = Mlist.update_nth index f_update tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> fail t.loc "Variable_core.bind_aux: expected the surrounding sequence"

(* [bind my_mark index fresh_name const is_ptr typ p_local t p]: applies [bind_aux] at trm [t] with path [p]. *)
let bind (my_mark : mark) (index : int) (fresh_name : var) (const : bool) (is_ptr : bool) (typ : typ option) (p_local : path) : Transfo.local =
  apply_on_path (bind_aux my_mark index fresh_name const is_ptr typ p_local)


(* [remove_get_operations_on_var x t]: removes all the get operation on variable [x]. *)
let remove_get_operations_on_var (x : var) (t : trm) : trm =
  let rec aux (belongs_to_get : bool) (t : trm) : bool * trm =
    let aux_false (t : trm) : trm =
      let _, t1 = aux false t in t1
      in
    match t.desc with
    | Trm_var (_, y) when y = x -> (true, t)
    | Trm_apps (_, [t1]) when is_get_operation t ->
      let r, t1' = aux true t1 in
      if r then (true, t1') else (false, trm_get t1')
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f)))}, [t1]) ->
      let r, t1' = aux belongs_to_get t1 in
      if r then (true, trm_struct_get ~typ:t.typ ~annot:t.annot t1' f) else (false, trm_struct_access ~typ:t.typ t1' f)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)))}, [t1; t2]) ->
      let r, t1' = aux belongs_to_get t1 in
      let _, t2' = aux false t2 in
      if r then (true, trm_array_get t1' t2') else (false, trm_array_access t1' t2')
    | _ -> false, trm_map aux_false t
   in
   snd (aux false t)

(* [remove_get_operations_on_var_temporary x t]: to be removed. *)
let rec remove_get_operations_on_var_temporary (x : var) (t : trm) : trm = (* ARTHUR *)
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get))}, [{desc = Trm_var (_,y);_}as ty]) when y = x -> ty
  | _ -> trm_map (remove_get_operations_on_var_temporary x) t

(* [Variable_to_const_abort]: exception raised by [from_to_const_aux]. *)
exception Variable_to_const_abort

(* [from_to_const_aux index t]: changes the mutability of a variable without explicit writes,
      [to_const] - if true, then the transformation will try to transform the targeted variable
        into a const variable and vice versa,
      [index] - the index of the targeted declaration inside its surrounding sequence,
      [t] - ast of the sequence that contains the targeted declaration. *)
let from_to_const_aux (to_const : bool) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->

    let lfront, dl, lback = Internal.get_item_and_its_relatives index tl in
    begin match dl.desc with
    | Trm_let (vk, (x, tx), init) ->
      let update_seq (new_dl : trm) (new_lback : trm mlist) (new_lfront : trm mlist) : trm =
        let new_tl = Mlist.merge lfront new_lback in
        let new_tl = Mlist.insert_at index new_dl new_tl in
        trm_seq ~annot:t.annot new_tl
      in
       begin match vk with
       | Var_immutable ->
        if to_const then t
          else  begin
            let init_val = match get_init_val init with
            | Some init1 -> init1
            | _ -> fail dl.loc "Variable_core.to_const_aux: const variables should always be initialized"
              in
            let init_type = get_inner_const_type tx in
            let new_dl = trm_pass_marks dl (trm_let_mut (x, init_type) init_val) in
            let new_lback = Mlist.map (Internal.subst_var x (trm_var_possibly_mut ~typ:(Some init_type) x)) lback in
            update_seq new_dl new_lback lfront
          end

       | Var_mutable ->
        if trm_has_cstyle Reference dl then fail dl.loc "Variable_core.from_to_const_aux: const references are not supported"
          else if not to_const then t
          else begin
            (* Search if there are any write operations on variable x *)
            Mlist.iter (fun t1 ->
              begin match t1.desc with
              | Trm_apps (_, [ls; _rs]) when is_set_operation t1 ->
                begin match ls.desc with
                | Trm_var (_, y) when y = x -> fail ls.loc "Variable_core.to_const_aux: variables with
                                     one or more write operations can't be converted to immutable ones"
                | _ -> if contains_occurrence x ls 
                            then fail ls.loc "Variable_core.to_const_aux: struct instances with 
                                    one or more write operations can't be conveted to immutable ones."
                            else ()
                end
              | _ -> ()
              end
            ) lback;
          (* replace all get(x) with x *)
            let init_val = match get_init_val init with
            | Some init1 -> init1
            | _ -> fail dl.loc "Variable_core.to_const_aux: can't convert to const a non intialized variable"
              in
            let init_type = get_inner_ptr_type tx in
            let new_dl = trm_pass_marks dl (trm_let_immut (x, init_type) init_val) in
            let new_lback = Mlist.map (fun t1 -> remove_get_operations_on_var x t1) lback in
            update_seq new_dl new_lback lfront
            end
       end
    | _ -> fail dl.loc "Variable_core.from_to_const_aux: the main target should point to a variable declaration"
    end
  | _ -> fail t.loc "Variable_core.from_to_const_aux: expected the sequence that contains the targeted declaration"

(* [from_to_const to_const index t p]: applies [from_to_const_aux] at trm [t] with path [p]. *)
let from_to_const (to_const : bool) (index : int) : Transfo.local =
  apply_on_path (from_to_const_aux to_const index )

(* [simpl_deref_aux t]: checks if [t] is of the form *(&b) or *(&b),
    if that's the case then simplify that expression and return it,
       [indepth] - search indepth for the targeted expressions,
       [t] - trm that represents one of the epxressions *(&b) or &( *b). *)
let simpl_deref_aux (indepth : bool) (t : trm) : trm =
  let aux = trm_simplify_addressof_and_get in
  if indepth then trm_map aux t else aux t

(* [simpl_deref indepth t p]: applies [simpl_deref_aux] at trm [t] with path [p]. *)
let simpl_deref (indepth : bool) : Transfo.local =
  apply_on_path (simpl_deref_aux indepth)

(* [ref_to_pointer_aux index t]: transforms the targeted declaration from a reference to a poitner,
      [index] - index of that targeted declaration in its surrounding block,
      [t] - ast of the sequence that contains the targeted declaration. *)
let ref_to_pointer_aux (index : int) (t : trm) : trm =
  let error = "Variable_core.ref_to_pointer_aux: expected the surrounding sequence of the targeted reference declaration." in
  let tl = trm_inv ~error trm_seq_inv t in
  let var_name = ref "" in
  let f_update (t : trm) : trm =
    match t.desc with
    | Trm_let (vk,( x, tx), init) when trm_has_cstyle Reference t ->
      var_name := x;
      let tx = get_inner_ptr_type tx in
      trm_let_mut (x, typ_ptr_generated tx) init
    | _ -> fail t.loc "Variable_core.ref_to_pointer_aux: expected a target to a variable declaration"
    in
  let f_update_further (t : trm) : trm =
    Internal.subst_var !var_name (trm_var_get !var_name) t in
  
  let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
  trm_seq ~annot:t.annot new_tl

(* [ref_to_pointer index t p]: applies [ref_to_pointer_aux] at trm [t] with path [p]. *)
let ref_to_pointer (index : int) : Transfo.local =
  apply_on_path (ref_to_pointer_aux index)

(* [ref_to_var_aux t]: converts a reference variable to a simple stack var variable
     [t] - ast of the refernce declaration *)
let ref_to_var_aux (t : trm) : trm =
  match t.desc with
  | Trm_let (vk, (x, tx), init) when trm_has_cstyle Reference t ->
    let t_annot = trm_rem_cstyle Reference t in
    let t_annot = trm_add_cstyle Stackvar t_annot in
    (trm_let ~annot:t_annot.annot vk (x, tx) (trm_new (get_inner_ptr_type tx) (trm_get init)))
  | _ -> fail t.loc "Variable_core.ref_to_var_aux: expected a target to a reference declaration"


(* [ref_to_var]: applies [ref_to_var_aux] at trm [t] with path [p]. *)
let ref_to_var : Transfo.local =
  apply_on_path (ref_to_var_aux )
