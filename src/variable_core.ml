open Ast
open Target

(* *********************************************************************************** 
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [fold_aux as_reference fold_at]: fold the variable declarations t
    params:
      as_reference: a flag for telling if the variable on the assignment 
        has the address operator or not
      fold_at: target where folding should be performed, if left empty 
        then folding is applied everywhere
      t: ast of the variable declaration
    return:
      updated ast 
*)
let fold_aux (as_reference : bool) (fold_at : target) (index : int) (t : trm) : trm=
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
            begin match vk with 
            | Var_immutable -> 
              if as_reference 
                then {dx with add = List.filter (fun x -> x <> Add_address_of_operator) dx.add}
                else dx
            | _ -> begin match dx.desc with 
                   | Trm_apps(_, [init]) -> 
                    if as_reference 
                      then {init with add = List.filter (fun x -> x <> Add_address_of_operator) init.add}
                      else init
                   | _ -> fail t.loc "fold_aux: expected a new operation"
                   end
            end in
        let lback = List.map(Internal.change_trm ~change_at:[fold_at] def_x t_x) lback
        (*
          def_x might have been replaced with x in the definition of x
          -> replace it again with def_x
         *)
        in

        trm_seq (lfront @ [d] @ lback)

     | _ -> fail t.loc "fold_decl: expected a variable declaration"
     end


  | _ -> fail t.loc "fold_aux: expected the surrounding sequence"

let fold (as_reference : bool) (fold_at : target) (index) : Target.Transfo.local =
  Target.apply_on_path(fold_aux as_reference fold_at index)


(* [inline_aux inline_at]: inline variable defined in term t
    params:
      delete_decl: delete or don't delete the declaration of the variable after inlining
      inline_at: target where inlining should be performed, if empty inlining is applied everywhere
      t: ast of the variable declaration
    return:
      updated ast
*)
let inline_aux (delete_decl : bool) (inline_at : target) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let dl, lback = Tools.split_list_at 1 lback in
    let dl = List.hd dl in
    begin match dl.desc with
    | Trm_let (vk, (x,tx), dx) ->
      let t_x = begin match vk with 
                | Var_immutable -> trm_var x
                | _ -> trm_apps ~annot:[Mutable_var_get] (trm_unop Unop_get) [trm_var x] 
                end in
      let def_x = 
      begin match vk with 
            | Var_immutable -> dx
            | _ -> begin match dx.desc with 
                   | Trm_apps(_, [init]) -> init
                   | _ -> fail t.loc "inline_aux: expected a new operation"
                   end
      end in
      let init = get_init_val dl in
      let lback = 
      begin match init.desc with 
      | Trm_struct field_init ->
        let tyid = Internal.get_typid_from_typ tx in
        let typid_to_typedef_map = Clang_to_ast.(!ctx_typedef) in
        let struct_def = Typ_map.find tyid typid_to_typedef_map in
        let field_list = fst (List.split (Internal.get_field_list struct_def)) in
        List.map (fun t1 ->
          List.fold_left2 (fun acc t2 f2 ->  Internal.change_trm ~change_at:[inline_at] 
            (trm_apps (trm_unop (Unop_struct_field_get f2)) [trm_var x]) t2 acc
          ) t1 field_init field_list) lback 
      | _ -> List.map (Internal.change_trm ~change_at:[inline_at] t_x def_x) lback 
      end in
      let tl =
        if delete_decl then lfront @ lback
        else lfront @ [dl] @ lback
      in
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "inline_aux: expected a variable declaration"
    end
  | _ -> fail t.loc "inline_aux: expected the surrounding sequence"


let inline (delete_decl : bool) (inline_at : target) (index : int) : Target.Transfo.local =
  Target.apply_on_path(inline_aux delete_decl inline_at index)

(* [rename_aux new_name index t] rename a variable, change its declaration
      and all its occurrences
   params:
    rename: a type covering both the case when a prefix is given or the list of variables to change
      together with their new name
   return:
    updated ast 
*)
let rename_aux (rename : rename) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    List.fold_left (fun acc t1 ->
        match t1.desc with
        | Trm_let (vk,(x, tx), init) ->
          begin match rename with 
          | Postfix post_fix ->
            let func = fun x -> x ^ post_fix in 
            let acc = Internal.change_trm t1 (trm_let vk ((func x), tx) init) acc in
            Internal.change_trm (trm_var x) (trm_var (func x)) acc
          | Rename_list list -> 
            if List.mem_assoc x list then
            begin 
            let new_var = List.assoc x list in
            let acc = Internal.change_trm t1 (trm_let vk (new_var, tx) init) acc in
             Internal.change_trm (trm_var x) (trm_var new_var) acc
            end
            else
              acc 
          end
        | _ -> acc
      ) t tl 
  | _ -> fail t.loc "rename_aux: expected the sequence block"

let rename (rename : rename) : Target.Transfo.local =
  Target.apply_on_path (rename_aux rename)

(* [init_detach_aux t]: replace an initialized variable declaration with an
    uninitialized declaration and an assignment.
    params:
      index: 
      t: ast of the surrounding sequence of the variable declaration
    return:
      the updated ast of the outer sequence which contains the declaration of the variable 
      and a set operations for that variable
*)
let init_detach_aux (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront,decl, lback  = Internal.get_trm_and_its_relatives index tl in
    begin match decl.desc with 
    | Trm_let(vk,(x, tx), init) ->
      begin match vk with
      | Var_immutable -> fail t.loc "init_detach_aux: const declarations cannot be detached"
      | _ ->
        let init =
          begin match init.desc with
          | Trm_apps(_,[init]) -> init
          | _ -> fail t.loc "init_detach_aux: expected a heap allocated variable declaration"
          end in
        let var_decl = trm_let vk (x, tx) (trm_prim (Prim_new tx)) in
        let var_assgn = trm_set (trm_var ~typ:(Some (get_inner_ptr_type tx)) x) {init with typ = (Some (get_inner_ptr_type tx))} in
        trm_seq ~annot:t.annot (lfront @ [var_decl; var_assgn] @ lback)
      end
    | _ -> fail decl.loc "init_detach_aux: variable could not be matched, make sure your path is correct"
    end
  | _ -> fail t.loc "init_detach_aux: expected the surrounding sequence"

let init_detach (index : int) : Target.Transfo.local =
  Target.apply_on_path(init_detach_aux index )


(* [init_attach_aux t]: replace an uninitialized variable declaration with an initialized one.
    params:
      const: a boolean to decide if the attached variable should be mutable or not
      t: ast of the surrounding sequence of the variable declaration
    return
      the updated ast of the outer sequence which contains now the initialized variable declaration
*)
let init_attach_aux (const : bool ) (index : int) (t : trm) : trm =
  let counter = ref 0 in
  match t.desc with 
  | Trm_seq tl ->
    let lfront, trm_to_change, lback = Internal.get_trm_and_its_relatives index tl in
    begin match trm_to_change.desc with 
    | Trm_let (_, (x, tx), _) ->
        let init_index = Tools.foldi (fun i acc t1 -> 
          match t1.desc with 
          | Trm_apps(_,[ls;_]) ->
            begin match ls.desc with 
            | Trm_var y when y = x -> 
              if !counter <= 1 then Some i else fail t1.loc "init_attach_aux: cases with more than one occurence are not supported"
            | _ -> acc
            end
          | _ -> acc
        ) None lback in
        let index1  = match init_index with 
        | Some index -> index
        | _ -> fail trm_to_change.loc (Tools.sprintf("init_attach_aux: no assignment was found to the given variable %s") x)
          in
        let lfront1, assgn_to_change, lback1 = Internal.get_trm_and_its_relatives index1 lback in
        begin match assgn_to_change.desc with 
        | Trm_apps(_, [_; rhs]) ->
          let vk = if const then Var_immutable else Var_mutable in
          let inner_type = 
          begin match tx.typ_desc with
          | Typ_ptr {ptr_kind=Ptr_kind_mut; inner_typ = ty} -> ty
          | _ -> tx
          end in
          let tx = if const then typ_const inner_type else typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut inner_type in
          let init = if const then rhs else (trm_apps (trm_prim (Prim_new inner_type)) [rhs]) in 
          let new_trm = trm_let vk (x, tx)  init in
          trm_seq ~annot:t.annot (lfront @ lfront1 @ [new_trm] @ lback1)
        | _ -> fail assgn_to_change.loc "init_attach: something wen't wrong"
        end
    | _ -> fail t.loc "init_attach_aux: target_doesn't point to the right trm, expected a trm_let"
    end
  | _ -> fail t.loc "init_attach_axu: expected the surrounding sequence"


let init_attach (const : bool) (index : int) : Target.Transfo.local =
  Target.apply_on_path(init_attach_aux const index )


(* [const_non_const_aux t]: transform a const declaration to a non-const one or vice-versa
    params:
      t: ast of the variable declaration 
    return:
      the updated ast of the declaration
*)
let const_non_const_aux (t : trm) : trm =
  match t.desc with
  | Trm_let (vk, (x,tx), init) ->
    begin match vk with
     (* If variable is a constant than whe remove the const and we perform the heap allocation  *)
    | Var_immutable ->
      trm_let Var_mutable (x, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut tx) (trm_apps (trm_prim ~loc: t.loc (Prim_new tx)) [init])
    | _ ->
      let var_type = begin match tx.typ_desc with
      | Typ_ptr {inner_typ = t; _} -> t
      | _ -> fail t.loc "const_non_const_aux: expected a pointer type"
      end
      in
      let var_init = begin match init.desc with
      | Trm_apps(_, [_; init]) -> init
      | _ -> fail t.loc "const_non_const_aux: expected a something of the form 'new ()'"
      end
      in
      trm_let Var_immutable (x,var_type) var_init
    end
  | _ -> fail t.loc "const_non_const_aux: variable declaration was not matched, make sure the path is correct"

let const_non_const : Target.Transfo.local =
  apply_on_path (const_non_const_aux)