open Ast

(* [inline_array_access array_var new_vars t]: change all the occurences of the array to variables,
    params:
      [array_var]: array_variable  to apply changes on
      [new_vars]: a list of variables, the variables at index i replaces and occurence of [array_var[i]]
      [t]: ast node located in the same level or deeper as the array declaration
    return:
        updated ast with the replaced array accesses to variable references. *)
let inline_array_access (array_var : var) (new_vars : vars) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_access));_}, [base; index]) ->
      begin match base.desc with
      | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [{desc = Trm_var (_, x)}]) when x = array_var ->
        begin match index.desc with
        | Trm_val (Val_lit (Lit_int i)) ->
          if i >= List.length new_vars
            then fail index.loc "inline_array_access: the number of variable provided should be consistent with the size of the targeted array"
            else (trm_var (List.nth new_vars i))
        | Trm_apps ({desc = Trm_var (_, "ANY"); _}, _) ->
          let nb_vars = List.length new_vars in
          trm_address_of (trm_apps (trm_var "CHOOSE") ((trm_lit (Lit_int nb_vars)) :: (List.map trm_var_get new_vars)))
        | _ -> fail index.loc "inline_array_access: only integer indices are supported"
        end
      | _ ->  trm_map aux t
      end
    | _ -> trm_map aux t
   in aux t

(* [to_variables_aux new_vars t]: tansform an array declaration into a list of variable declarations
      the list of variables should be entered by the user. The number of variables should correspond to
      the size of the arrys. The variable at index i in [new_vars] will replace the array occurrence
      at index i
    params:
      (new_vars]: a list of strings of length equal to the size of the array
      [index]: index of the instruction inside the sequence
      [t]: ast of the surrounding sequence of the array declaration
    return:
      updated ast of the outer sequence with the replaced declarations and all changed accesses.
*)
let to_variables_aux (new_vars : vars) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, d, lback = Internal.get_trm_and_its_relatives index tl in
    let array_name = begin match (decl_name d) with
                     | Some nm -> nm
                     | None -> fail t.loc "to_variables_aux: could don't find the name of the array declaration"
                     end in
    let var_decls = begin match d.desc with
    | Trm_let (_, (_ , tx), init) ->
      begin match (get_inner_ptr_type tx).typ_desc with
      | Typ_array (t_var,_) ->
       begin match t_var.typ_desc with
       | Typ_constr (y, tid, _) ->
        List.map(fun x ->
          trm_let_mut ~annot:d.annot (x, typ_constr y ~tid) (trm_uninitialized ~loc:init.loc ()) ) new_vars
       | Typ_var (y, tid) ->
        List.map(fun x ->
          trm_let_mut ~annot:d.annot (x, typ_constr y ~tid) (trm_uninitialized ~loc:init.loc ()) ) new_vars
       | _ ->
        List.map(fun x ->
          trm_let_mut ~annot:d.annot (x, t_var) (trm_uninitialized ~loc:init.loc ())) new_vars
       end
      | _ -> fail t.loc "to_variables_aux: expected an array type"
      end
    | _ -> fail t.loc "to_variables_aux: expected a variable declaration"
    end
    in
    let lback = Mlist.map (inline_array_access array_name new_vars) lback in
    let new_tl = Mlist.merge lfront lback in
    let tl = Mlist.insert_sublist_at index var_decls new_tl in
    trm_seq ~annot:t.annot ~loc:t.loc tl
  | _ -> fail t.loc "to_variables_aux: expected the outer sequence of the targeted trm"

let to_variables (new_vars : vars) (index : int): Target.Transfo.local =
  Target.apply_on_path (to_variables_aux new_vars index)

(* [apply_tiling base_type block_name b x]: Change all the occurences of the array to the tiled form
    params:
      [base_type]: type of the array
      [block_name]: new name for the array
      [b]: the size of the tile
      [x]: typvar
      [t]: ast node located in the same level or deeper as the array declaration
    assumptions:
    - if x is ty*, each array of type x is allocated through a custom function:
      x a = my_alloc(nb_elements, size_element)
    - x is not used in function definitions, but only in var declarations
    - for now: in any case, the number of elements is divisible by b
   return:
      updated ast nodes which are in the same level with the array declaration or deeper.
*)
let rec apply_tiling (base_type : typ) (block_name : typvar) (b : trm) (x : typvar) (t : trm) : trm =
  let aux = apply_tiling base_type block_name b x in
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get))}, [arg]) ->
    begin match arg.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_access))}, [base;index]) ->
      begin match base.typ with
      | Some {typ_desc = Typ_constr (y,_, _); _} when y = x ->
          get_array_access (get_array_access base (trm_div index b)) (trm_mod index b)
      | _ ->
        trm_map aux t
      end
    | _ -> trm_map aux t
    end
  | _ -> trm_map aux t

(* [tile_aux: name block_name b x t]: transform an array declaration from a normal shape into a tiled one,
    then call apply_tiling to change all the array occurrences into the correct form.
    params:
      [block_name]: the name of the arrays representing one tile
      [block_size]: the size of the tile
      [index]: the index of the instruction inside the sequence
      [t]: ast of the outer sequence containing the array declaration
    return:
      updated ast of the surrounding sequence with the new tiled declaration and correct array accesses based on the new tiled form.

    t[N] -> t[N/B][B]  where t has the targeted type
    t[i] -> t[i/B][i%B]
*)
let tile_aux (block_name : typvar) (block_size : var) (index: int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, d, lback = Internal.get_trm_and_its_relatives index tl in
    let base_type_name, base_type =
    begin match d.desc with
    | Trm_typedef td ->
      begin match td.typdef_body with
      | Typdef_alias typ ->
        begin match typ.typ_desc with
        | Typ_ptr {inner_typ = ty;_} -> td.typdef_tconstr, ty
        | Typ_array(ty, _) -> td.typdef_tconstr, ty
        | _ -> fail d.loc "tile_aux: expected array or pointer type"
        end
      | _ -> fail d.loc "tile_aux: expected a typedef abbrevation"
      end
    | _ -> fail d.loc "tile_aux: expected a trm_typedef"
    end
    in
    let block_name = if block_name = "" then base_type_name ^ "_BLOCK" else block_name in
    (*
    replace sizeof(base_type) with sizeof(block_name)
    if another term is used for size: use b * t_size
   *)
    let new_size (t_size : trm) : trm =
      if AstC_to_c.ast_to_string t_size =
         "sizeof(" ^ AstC_to_c.typ_to_string base_type ^ ")"
      then trm_var ("sizeof(" ^ block_name ^ ")")
      else trm_apps (trm_binop Binop_mul) [trm_var block_size; t_size]
    in
    let new_alloc (t_alloc : trm) : trm =
      match t_alloc.desc with
      (* expectation: my_alloc(nb_elements, size_element) *)
      | Trm_apps (t_alloc_fun, [t_nb_elts; t_size_elt]) ->
         (* goal: my_alloc(nb_elements / b, b * size_element) *)
         let t_nb_elts = trm_apps (trm_binop Binop_div) [t_nb_elts; trm_var block_size] in
         let t_size_elt = new_size t_size_elt in
         trm_apps t_alloc_fun [t_nb_elts; t_size_elt]
      (* there's possibly a cast first *)
      | Trm_apps (t_cast,
                  [{desc = Trm_apps (t_alloc_fun,
                                     [t_nb_elts; t_size_elt]); _}]) ->
         let t_nb_elts = trm_apps (trm_binop Binop_div) [t_nb_elts; trm_var block_size] in
         let t_size_elt = new_size t_size_elt in
         trm_apps t_cast [trm_apps t_alloc_fun [t_nb_elts; t_size_elt]]
      | _ -> fail t.loc "new_alloc: expected array allocation"
    in
    let array_decl = begin match d.desc with
    | Trm_typedef td ->
      begin match td.typdef_body with
      | Typdef_alias ty  ->
         begin match ty.typ_desc with
        | Typ_ptr {inner_typ = ty;_} ->
           (* ty* becomes (ty[])* *)
           trm_seq_no_brace
              [
                trm_typedef {
                  td with typdef_tconstr = block_name;
                  typdef_body = Typdef_alias (typ_array ty (Trm (trm_var block_size)))};
                trm_typedef {
                  td with typdef_tconstr = td.typdef_tconstr;
                  typdef_body = Typdef_alias (typ_ptr Ptr_kind_mut (typ_constr block_name ~tid:td.typdef_typid ))}]
        | Typ_array (ty, s) ->
           (* ty[s] becomes ty[s/b][b] *)
           begin match s with
           | Undefined -> fail t.loc "tile_aux: array size must be provided"
           | Const n ->
              let n_div_b =
                trm_apps (trm_binop Binop_div) [trm_lit (Lit_int n); trm_var block_size]
              in
              let tid = next_typconstrid () in
              trm_seq_no_brace
                [
                  trm_typedef {
                    td with typdef_tconstr = block_name;
                    typdef_body = Typdef_alias (typ_array ty (Trm (trm_var block_size)))};
                  trm_typedef{
                    td with typdef_tconstr = td.typdef_tconstr;
                    typdef_body = Typdef_alias (typ_array (typ_constr block_name ~tid) (Trm n_div_b))}]
           | Trm t' ->
              let t'' = trm_apps (trm_binop Binop_div) [t'; trm_var block_size] in
              let tid = next_typconstrid () in
              trm_seq_no_brace
                [
                  trm_typedef {
                    td with typdef_tconstr = block_name;
                    typdef_body = Typdef_alias (typ_array ty (Trm (trm_var block_size)))};

                  trm_typedef {
                    td with typdef_tconstr = td.typdef_tconstr;
                    typdef_body = Typdef_alias (typ_array (typ_constr block_name ~tid) (Trm t''))}]
           end
        | _ -> fail t.loc "tile_aux: expected array or pointer type declaration"
        end
      | _ -> fail t.loc "tile_aux: no enums expected"
      end

    | Trm_let (Var_mutable, (y,ty), init) when y = base_type_name ->
        begin match ty.typ_desc with
        | Typ_ptr {inner_typ = {typ_desc = Typ_constr (y, _, _); _};_} when y = base_type_name ->
          trm_let Var_mutable ~annot:d.annot (y, ty) init
        | _ -> fail t.loc "tile_aux: expected a pointer because of heap allocation"
        end
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
              [lhs; rhs]) ->
        (* lhs should have type x *)
        begin match lhs.typ with
        | Some {typ_desc = Typ_constr (y, _, _); _} when y = base_type_name ->
           trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement
             ~typ:t.typ (trm_binop Binop_set) [lhs; new_alloc rhs]
        | _ -> trm_map (apply_tiling base_type block_name (trm_var block_size) base_type_name) t
        end
    | _-> fail t.loc "tile_aux: expected a declaration"
      end

    in
    let lback = Mlist.map (apply_tiling base_type block_name (trm_var block_size) base_type_name) lback in
    let new_tl = Mlist.merge lfront lback in
    let new_tl = Mlist.insert_at index array_decl new_tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> fail t.loc "tile_aux: expected the surrounding sequence of the targeted trm"

let tile (block_name : typvar) (block_size : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (tile_aux block_name block_size index)


(* [apply_swapping x t]: change all the occurrences of the array to the swapped form.
    params:
      [x]: typvar
      [t]: an ast node which on the same level as the array declaration or deeper.
    return:
      updated ast nodes which are in the same level with the array declaration or deeper.
 *)

let rec apply_swapping (x : typvar) (t : trm) : trm =
  let aux = apply_swapping x in
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [arg]) ->
    begin match arg.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));_}, [base; index]) ->
      begin match get_array_access_inv base with
      | Some (base1, index1) ->
        begin match base1.typ with
        | Some {typ_desc = Typ_constr (y,_, _); _} when y = x -> get_array_access (get_array_access base1 index) index1
        | _ -> trm_map aux t
        end
      | None -> trm_map aux t
      end
    | _ -> trm_map aux t
    end
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));_}, [base; index]) ->
      begin match get_array_access_inv base with
      | Some (base1, index1) ->
        array_access (get_array_access base1 index) index1
      | None -> trm_map aux t
      end
  | _ -> trm_map aux t


(* [swap_aux index t]: transform an array declaration to a swaped one, Basically the bounds will swap
     places in the array declaration, and the indices will swap places on all the array occurrences.
    params:
      [index]: used to find the instruction inside the sequence
      [t]: ast of the surrounding sequence if the array declaration
    assumption: the name of the array is not used in fun declarations
      -> to swap the first dimensions of a function argument, use swap_coordinates
      on the array on which the function is called: a new function with the
      appropriate type is generated
      function copies are named with name
    return: updated outer sequence with the replaced declarations and all swapped accesses.

    TODO: doc should say something like
     aiming for typedef t[N][M] foo;
     and changing   v[i][j]  where v has type foo
      with v[j][i]
*)
let swap_aux (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    (* TODO:
       let trm_seq_update_def_and_rest tseq index (f_def : trm->trm) (f_rest : trm->trm) =
          trm_seq_updates_def_and_rest tseq index (fun t -> [f_def t]) f_rest

       let trm_seq_updates_def_and_rest tseq index (f_def : trm->trm list) (f_rest : trm->trm)
         f_def applies to the item at index in tseq and produces one or several terms,
         f_rest applies to all the items past this one
         the output is thus the trm:
            Trm_seq (front_items @ f_def def_item @ List.map f_rest back_items)

            how to use:
        let process_def t = ..
        let process_inscope t = ..
        trm_seq_update_def_and_rest ...
    *)
    let lfront, d, lback = Internal.get_trm_and_its_relatives index tl in
    begin match d.desc with
      | Trm_typedef td -> (* TODO: trm_typedef_inv and trm_typedef_alias_inv *)
        begin match td.typdef_body with
        | Typdef_alias ty ->
           let rec swap_type (ty : typ) : typ =
            match ty.typ_desc with
            | Typ_array ({typ_desc = Typ_array (ty', s'); typ_annot; typ_attributes},
                        s) ->
              begin match ty'.typ_desc with
              (* we look for the 2 first coordinates… *)
              | Typ_array _ ->
                  let t' =
                    swap_type {typ_desc = Typ_array (ty', s'); typ_annot; (* TODO: why rec call? *)
                              typ_attributes}
                  in
                  {typ_desc = Typ_array (t', s); typ_annot = ty.typ_annot;
                  typ_attributes = ty.typ_attributes}
              (* once we reach them, we swap them *)
              | _ ->
                  {typ_desc = Typ_array ({typ_desc = Typ_array (ty', s);
                                        typ_annot = ty.typ_annot;
                                        typ_attributes = ty.typ_attributes}, s');
                  typ_annot; typ_attributes}
              end
            | _ -> fail None ("swap_type: must be an array")
          in
        let new_decl =
        trm_typedef ~annot: t.annot ~loc: t.loc ~is_statement:t.is_statement
          {td with typdef_body = Typdef_alias (swap_type ty)}
        in
        let lback = Mlist.map (apply_swapping td.typdef_tconstr) lback in
        let new_tl = Mlist.merge lfront lback in
        let new_tl = Mlist.insert_at index new_decl new_tl in
        trm_seq ~annot:t.annot new_tl
        | _ -> fail t.loc "swap_aux: expected a declaration"
        end
      | _ -> fail t.loc "swap_aux: expected the typedef"
    end
  | _ -> fail t.loc "swap_aux: expected the surrounding sequence of the targeted trm"

let swap (index : int) : Target.Transfo.local =
  Target.apply_on_path (swap_aux index)


(* [aos_to_soa_aux t ] : Transform an array of structures to a structure of arrays
    params:
      [index]: the index of the array declaration inside the surrounding sequence
      [t]: ast of the outer sequence containing the array of structures declaration
    return:
      updated ast of the surrounding sequence wuth the new changed declaration and occurences
*)
(* TODO: Reimplement it from scratch *)
let aos_to_soa_aux (struct_name : typvar) (sz : var) (t : trm) : trm =
  let rec aux (global_trm : trm) (t : trm) : trm =
    match t.desc with
    (* LATER: document  E.G.  matching (array_access(struct_access(t, f), index)); ... *)
    | Trm_apps(_,[get_base]) when is_access t  ->
      begin match get_base.desc with
      | Trm_apps (f, [base]) ->
         begin match f.desc with
         | Trm_val (Val_prim (Prim_unop (Unop_struct_access _)))
           | Trm_val (Val_prim (Prim_unop (Unop_struct_get _))) ->
            begin match base.desc with
            | Trm_apps (f', [base'; index]) ->
               begin match f'.desc with
               | Trm_val (Val_prim (Prim_binop Binop_array_access))
                 | Trm_val (Val_prim (Prim_binop Binop_array_get)) ->
                  (*
                    swap accesses only if the type of base' is x (or x* in case of
                    an access on a heap allocated variable)
                   *)

                   (*
                     LATER: Arthur write better specification for the changes
                      <<t>> [i].x
                   *)
                   (* TODO ARTHUR *)

                  let base'  = match base'.desc with
                  | Trm_apps (_, [base'']) when is_get_operation base'  -> base''
                  | _ -> base'
                   in
                  let index  = match index.desc with
                  | Trm_apps (_, [index']) when is_get_operation index -> index'
                  | _ -> index
                   in
                  begin match base'.typ with
                  | Some {typ_desc = Typ_array({typ_desc = Typ_constr (y, _, _);_}, _);_} when y = struct_name ->
                     let base' = aux global_trm base' in
                     let index = aux global_trm index in
                     (* keep outer annotations *)
                     trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement
                       ~typ:t.typ f' [trm_apps f [base']; index]
                  | Some {typ_desc = Typ_constr (y, _, _); _} when y = struct_name ->
                     (* x might appear both in index and in base' *)
                     let base' = aux global_trm base' in
                     let index = aux global_trm index in
                     (* keep outer annotations *)
                     trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement
                       ~typ:t.typ f' [trm_apps f [base']; index]
                  | Some {typ_desc = Typ_ptr {inner_typ = {typ_desc = Typ_constr (y, _, _); _}; _};_}
                       when y = struct_name ->
                     (* x might appear both in index and in base' *)
                     let base' = aux global_trm base' in
                     let index = aux global_trm index in
                     (* keep outer annotations *)
                     trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement
                       ~typ:t.typ f' [trm_apps f [base']; index]
                  | _ -> trm_map (aux global_trm) t
                  end

               | _ -> trm_map (aux global_trm) t
               end
            | _ -> trm_map (aux global_trm) t
            end
         | _ -> trm_map (aux global_trm) t
         end
      | _ -> trm_map (aux global_trm) t
      end
      (* LATER: arthur: test the use of exceptions for structuring this code *)

    | Trm_typedef td when td.typdef_tconstr = struct_name ->
      begin match td.typdef_body with
      | Typdef_prod (tn, s) ->
        let s = List.map( fun (x, typ) -> (x, typ_array (typ) (Trm (trm_var sz)))) s in
        trm_typedef {td with typdef_body = Typdef_prod (tn, s)}
      | Typdef_alias ty ->
          begin match ty.typ_desc with
          | Typ_array (a, _)->
            begin match a.typ_desc with
            | Typ_constr (sn, _, _) when sn = struct_name-> trm_typedef {td with typdef_body  = Typdef_alias a}

            | _ -> trm_map(aux global_trm) t
            end

          | _ -> trm_map(aux global_trm) t
          end
      | _ -> fail t.loc "aos_to_soa_aux: expected a typedef struct"
      end
    | Trm_typedef td ->
        begin match td.typdef_body with
        | Typdef_alias ty ->
          begin match ty.typ_desc with
          | Typ_array (a, _)->
            begin match a.typ_desc with
            | Typ_constr (sn, _, _) when sn = struct_name-> trm_typedef {td with typdef_body  = Typdef_alias a}

            | _ -> trm_map(aux global_trm) t
            end

          | _ -> trm_map(aux global_trm) t
          end
        | _ -> trm_map(aux global_trm) t
        end

    | Trm_let (vk, (n, dx), init) ->
       begin match dx.typ_desc with
       | Typ_ptr {inner_typ = ty;_} ->
        begin match ty.typ_desc with
        | Typ_array (a, _) ->
          begin match a.typ_desc with
          | Typ_constr (sn,_, _) when sn = struct_name ->
            trm_let_mut ~annot:t.annot (n, a) (trm_uninitialized ~loc:init.loc ())
          | _ -> trm_map (aux global_trm) t
          end
        | _ -> trm_map (aux global_trm) t
        end
       | _ -> trm_map (aux global_trm) t
       end
    | _ -> trm_map (aux global_trm) t
  in aux t t

let aos_to_soa (tv : typvar) (sz : var): Target.Transfo.local =
  Target.apply_on_path(aos_to_soa_aux tv sz)

(* [set_explicit_aux t]: transoform an initialized array declaration into a list of write operations
      for each one of its cells
      params:
        [t]: ast of the array declaration
      return:
        the ast of the uninitialized array declaration and a list of write operations
*)
let set_explicit_aux (t : trm) : trm =
  match t.desc with
  | Trm_let (vk, (x, tx), init) ->
    let init = match get_init_val init with
    | Some init -> init
    | None -> fail t.loc "set_explicit_aux: could not get the initialization trms for the targeted array declaration" in
    begin match init.desc with
    | Trm_array tl ->
      let array_set_list =
      List.mapi ( fun i t1 ->
        trm_set (trm_apps (trm_binop (Binop_array_access)) [trm_var_get x;trm_int i]) t1
      ) (Mlist.to_list tl) in
      let new_decl = trm_let_mut ~annot:t.annot (x, (get_inner_ptr_type tx)) (trm_uninitialized ~loc:init.loc ()) in
      trm_seq_no_brace ([new_decl] @ array_set_list)
    | _ -> fail init.loc "set_explicit_aux: expected an array initialization"
    end
  | _ -> fail t.loc "set_explicit_aux: expected an array declaration"


let set_explicit : Target.Transfo.local =
  Target.apply_on_path (set_explicit_aux )