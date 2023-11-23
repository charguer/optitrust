open Prelude

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
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_access));_}, [base; index], _) ->
      begin match base.desc with
      | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [{desc = Trm_var (_, x)}], _) when (var_eq x array_var) ->
        begin match index.desc with
        | Trm_val (Val_lit (Lit_int i)) ->
          if i >= List.length new_vars
            then trm_fail index "Arrays_core.inline_array_access: the number of variable provided should be consistent with the size of the targeted array"
            else (trm_var (List.nth new_vars i))
        | Trm_apps ({desc = Trm_var (_, x); _}, _, _) when var_has_name x "ANY" ->
          let nb_vars = List.length new_vars in
          trm_address_of (trm_apps (trm_var (name_to_var "CHOOSE")) ((trm_lit (Lit_int nb_vars)) :: (List.map trm_var_get new_vars)))
        | _ -> trm_fail index "Arrays_core.inline_array_access: only integer indices are supported"
        end
      | _ ->  trm_map aux t
      end
    | _ -> trm_map aux t
   in aux t

(* [to_variables_aux new_vars t]: tansform an array declaration into a list of variable declarations
      the list of variables should be entered by the user. The number of variables should correspond to
      the size of the arrys. The variable at index i in [new_vars] will replace the array occurrence
      at index i.
      (new_vars] - a list of strings of length equal to the size of the array,
      [index] - index of the instruction inside the sequence,
      [t] - ast of the surrounding sequence of the array declaration. *)
let to_variables_aux (new_vars : string list) (index : int) (t : trm) : trm =
  let new_vars = List.map Trm.new_var new_vars in
  match t.desc with
  | Trm_seq tl ->
    let array_var = ref dummy_var in
    let f_update_at (t : trm) : trm =
      begin match t.desc with
        | Trm_let (_, (x , tx), init) ->
          array_var := x;
          begin match (get_inner_ptr_type tx).typ_desc with
          | Typ_array (t_var,_) ->
            begin match t_var.typ_desc with
            | Typ_constr (y, tid, _) ->
              trm_seq_nobrace_nomarks (
                List.map(fun x ->
                trm_let_mut ~annot:t.annot (x, typ_constr y ~tid) (trm_uninitialized ?loc:init.loc ()) ) new_vars)
            | Typ_var (y, tid) ->
              trm_seq_nobrace_nomarks (
                 List.map(fun x ->
                 trm_let_mut ~annot:t.annot (x, typ_constr ([], y) ~tid) (trm_uninitialized ?loc:init.loc ()) ) new_vars)
            | _ ->
              trm_seq_nobrace_nomarks (
              List.map(fun x ->
              trm_let_mut ~annot:t.annot (x, t_var) (trm_uninitialized ?loc:init.loc ())) new_vars)
            end
          | _ -> trm_fail t "Arrays_core.to_variables_aux: expected an array type"
          end
        | _ -> trm_fail t "Arrays_core.to_variables_aux: expected a variable declaration"
        end
      in

    let f_update_further (t : trm) : trm =
      inline_array_access !array_var new_vars t
      in
    let new_tl = Mlist.update_at_index_and_fix_beyond index f_update_at f_update_further tl in

    trm_seq ~annot:t.annot ?loc:t.loc new_tl

  | _ -> trm_fail t "Arrays_core.to_variables_aux: expected the outer sequence of the targeted trm"


(* [to_variables new_vars index t p]: applies [to_variables_aux] at trm [t] with path [p]. *)
let to_variables (new_vars : string list) (index : int): Target.Transfo.local =
  Target.apply_on_path (to_variables_aux new_vars index)

(* [apply_tiling base_type block_name b x]: changes all the occurences of the array to the tiled form,
      [base_type] - type of the array
      [block_name] - new name for the array
      [b] - the size of the tile
      [x] - typvar
      [t] - ast node located in the same level or deeper as the array declaration

    assumptions:
    - if x is ty*, each array of type x is allocated through a custom function:
      x a = my_alloc(nb_elements, size_element)
    - x is not used in function definitions, but only in var declarations
    - for now: in any case, the number of elements is divisible by b. *)
let rec apply_tiling (base_type : typ) (block_name : typvar) (b : trm) (x : typvar) (t : trm) : trm =
  let aux = apply_tiling base_type block_name b x in
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get))}, [arg], _) ->
    begin match arg.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_access))}, [base;index], _) ->
      begin match base.typ with
      | Some {typ_desc = Typ_constr (y,_, _); _} when (snd y) = x ->
          get_array_access (get_array_access base (trm_div index b)) (trm_mod index b)
      | _ ->
        trm_map aux t
      end
    | _ -> trm_map aux t
    end
  | _ -> trm_map aux t

(* [tile_aux: name block_name b x t]: transform an array declaration from a normal shape into a tiled one,
    then call apply_tiling to change all the array occurrences into the correct form.
      [block_name] - the name of the arrays representing one tile,
      [block_size] - the size of the tile,
      [index] - the index of the instruction inside the sequence,
      [t] - ast of the outer sequence containing the array declaration.

   Ex:
    t[N] -> t[N/B][B]  where t has the targeted type
    t[i] -> t[i/B][i%B] *)
let tile_aux (block_name : typvar) (block_size : var) (index: int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, d, lback = Mlist.get_item_and_its_relatives index tl in
    let base_type_name, base_type =
    begin match d.desc with
    | Trm_typedef td ->
      begin match td.typdef_body with
      | Typdef_alias typ ->
        begin match typ.typ_desc with
        | Typ_ptr {inner_typ = ty;_} -> td.typdef_tconstr, ty
        | Typ_array(ty, _) -> td.typdef_tconstr, ty
        | _ -> trm_fail d "Arrays_core.tile_aux: expected array or pointer type"
        end
      | _ -> trm_fail d "Arrays_core.tile_aux: expected a typedef abbrevation"
      end
    | _ -> trm_fail d "Arrays_core.tile_aux: expected a trm_typedef"
    end
    in
    let block_name = if block_name = "" then base_type_name ^ "_BLOCK" else block_name in
    (* replace sizeof(base_type) with sizeof(block_name) if another term is used for size: use b * t_size *)
    let new_size (t_size : trm) : trm =
      if AstC_to_c.ast_to_string t_size =
         "sizeof(" ^ AstC_to_c.typ_to_string base_type ^ ")"
      then trm_toplevel_var ("sizeof(" ^ block_name ^ ")")
      else trm_apps (trm_binop Binop_mul) [trm_var block_size; t_size]
    in
    let new_alloc (t_alloc : trm) : trm =
      match t_alloc.desc with
      (* expectation: my_alloc(nb_elements, size_element) *)
      | Trm_apps (t_alloc_fun, [t_nb_elts; t_size_elt], _) ->
         (* goal: my_alloc(nb_elements / b, b * size_element) *)
         let t_nb_elts = trm_apps (trm_binop Binop_div) [t_nb_elts; trm_var block_size] in
         let t_size_elt = new_size t_size_elt in
         trm_apps t_alloc_fun [t_nb_elts; t_size_elt]
      (* there's possibly a cast first *)
      | Trm_apps (t_cast,
                  [{desc = Trm_apps (t_alloc_fun,
                                     [t_nb_elts; t_size_elt], _); _}], _) ->
         let t_nb_elts = trm_apps (trm_binop Binop_div) [t_nb_elts; trm_var block_size] in
         let t_size_elt = new_size t_size_elt in
         trm_apps t_cast [trm_apps t_alloc_fun [t_nb_elts; t_size_elt]]
      | _ -> trm_fail t "Arrays_core.tile_aux: expected array allocation"
    in
    let array_decl = begin match d.desc with
    | Trm_typedef td ->
      begin match td.typdef_body with
      | Typdef_alias ty  ->
         begin match ty.typ_desc with
        | Typ_ptr {inner_typ = ty;_} ->
           (* ty* becomes (ty[])* *)
           trm_seq_nobrace_nomarks
              [
                trm_typedef {
                  td with typdef_tconstr = block_name;
                  typdef_body = Typdef_alias (typ_array ty (Trm (trm_var block_size)))};
                trm_typedef {
                  td with typdef_tconstr = td.typdef_tconstr;
                  typdef_body = Typdef_alias (typ_ptr Ptr_kind_mut (typ_constr ([], block_name) ~tid:td.typdef_typid ))}]
        | Typ_array (ty, s) ->
           (* ty[s] becomes ty[s/b][b] *)
           begin match s with
           | Undefined -> trm_fail t "Arrays_core.tile_aux: array size must be provided"
           | Const n ->
              let n_div_b =
                trm_apps (trm_binop Binop_div) [trm_lit (Lit_int n); trm_var block_size]
              in
              let tid = next_typconstrid () in
              trm_seq_nobrace_nomarks
                [
                  trm_typedef {
                    td with typdef_tconstr = block_name;
                    typdef_body = Typdef_alias (typ_array ty (Trm (trm_var block_size)))};
                  trm_typedef{
                    td with typdef_tconstr = td.typdef_tconstr;
                    typdef_body = Typdef_alias (typ_array (typ_constr ([], block_name) ~tid) (Trm n_div_b))}]
           | Trm t' ->
              let t'' = trm_apps (trm_binop Binop_div) [t'; trm_var block_size] in
              let tid = next_typconstrid () in
              trm_seq_nobrace_nomarks
                [
                  trm_typedef {
                    td with typdef_tconstr = block_name;
                    typdef_body = Typdef_alias (typ_array ty (Trm (trm_var block_size)))};

                  trm_typedef {
                    td with typdef_tconstr = td.typdef_tconstr;
                    typdef_body = Typdef_alias (typ_array (typ_constr ([], block_name) ~tid) (Trm t''))}]
           end
        | _ -> trm_fail t "Arrays_core.tile_aux: expected array or pointer type declaration"
        end
      | _ -> trm_fail t "Arrays_core.tile_aux: no enums expected"
      end

    | Trm_let (Var_mutable, (y,ty), init) when var_has_name y base_type_name ->
        begin match ty.typ_desc with
        | Typ_ptr {inner_typ = {typ_desc = Typ_constr (yc, _, _); _};_} when typconstr_has_name yc base_type_name ->
          trm_let Var_mutable ~annot:d.annot (y, ty) init
        | _ -> trm_fail t "Arrays_core.tile_aux: expected a pointer because of heap allocation"
        end
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
              [lhs; rhs], _) ->
        (* lhs should have type x *)
        begin match lhs.typ with
        | Some {typ_desc = Typ_constr (y, _, _); _} when (typconstr_has_name y base_type_name) ->
           trm_apps ~annot:t.annot ?loc:t.loc
             ?typ:t.typ (trm_binop Binop_set) [lhs; new_alloc rhs]
        | _ -> trm_map (apply_tiling base_type block_name (trm_var block_size) base_type_name) t
        end
    | _-> trm_fail t "Arrays_core.tile_aux: expected a declaration"
      end
    in
    let lback = Mlist.map (apply_tiling base_type block_name (trm_var block_size) base_type_name) lback in
    let new_tl = Mlist.merge lfront lback in
    let new_tl = Mlist.insert_at index array_decl new_tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> trm_fail t "Arrays_core.tile_aux: expected the surrounding sequence of the targeted trm"

let tile (block_name : typvar) (block_size : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (tile_aux block_name block_size index)


(* [apply_swapping x t]: swaps all array accesses that have type [t],
      [x] - typvar,
      [t] - an ast node which on the same level as the array declaration or deeper. *)
let rec apply_swapping (x : typvar) (t : trm) : trm =
  let aux = apply_swapping x in
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [arg], _) ->
    begin match arg.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));_}, [base; index], _) ->
      begin match get_array_access_inv base with
      | Some (base1, index1) ->
        begin match base1.typ with
        | Some {typ_desc = Typ_constr (y,_, _); _} when (typconstr_has_name y x) -> get_array_access (get_array_access base1 index) index1
        | _ -> trm_map aux t
        end
      | None -> trm_map aux t
      end
    | _ -> trm_map aux t
    end
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));_}, [base; index], _) ->
      begin match get_array_access_inv base with
      | Some (base1, index1) ->
        begin match base1.typ with
        | Some {typ_desc = Typ_constr (y, _, _)} when (typconstr_has_name y x) -> array_access (get_array_access base1 index) index1
        | _ -> trm_map aux t
        end
      | None -> trm_map aux t
      end
  | _ -> trm_map aux t


(* [swap_aux index t]: swap the dimensions of an array declaration,
     [index] - index of the array declaration on its surrouding sequence,
     [t] - AST of the surrouding sequence of the targeted array declaration. *)
let swap_aux (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->

    let rec swap_type (ty : typ) : typ =
      match ty.typ_desc with
      | Typ_array ({typ_desc = Typ_array (ty', s'); typ_annot; typ_attributes},
                  s) ->
        begin match ty'.typ_desc with
        (* we look for the 2 first coordinates… *)
        | Typ_array _ ->
            let t' =
              swap_type {typ_desc = Typ_array (ty', s'); typ_annot;
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
      | _ -> trm_fail t "Arrays_core.swap_type: the main target should point an an array declaration"
      in

    let f_update (t : trm) : trm =
      match t.desc with
      | Trm_typedef td ->
        begin match td.typdef_body with
        | Typdef_alias ty ->
          trm_typedef ~annot:t.annot ?loc:t.loc {td with typdef_body = Typdef_alias (swap_type ty)}
        | _ -> trm_fail t "Arrays_core.swap_aux: expected a type alias definition."
        end
      | _ -> trm_fail t "Arrays_core.swap_aux: expected the typedef instruction."

      in

    let f_update_further (t : trm) : trm =
      let td = Mlist.nth tl index in
      match td.desc with
      | Trm_typedef td -> apply_swapping td.typdef_tconstr t
      | _ -> trm_fail t "Arrays_core.swap_aux: expected a target to a type definition"
      in
    let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> trm_fail t "swap_aux: expected the surrounding sequence of the targeted trm"

(* [swap index t p]: applies [swap_aux] at trm [t] with path [p]. *)
let swap (index : int) : Target.Transfo.local =
  Target.apply_on_path (swap_aux index)


(* [aos_to_soa_aux t ] : transforms an array of structures to a structure of arrays
      [index] - the index of the array declaration inside the surrounding sequence
      [t] - ast of the outer sequence containing the array of structures declaration. *)
(* TODO: Reimplement it from scratch *)
let aos_to_soa_aux (struct_name : typvar) (sz : var) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    (* LATER: document  E.G.  matching (array_access(struct_access(t, f), index)); ... *)
    | Trm_apps(_,[get_base], _) when is_access t  ->
      begin match get_base.desc with
      | Trm_apps (f, [base], _) ->
         begin match f.desc with
         | Trm_val (Val_prim (Prim_unop (Unop_struct_access _)))
           | Trm_val (Val_prim (Prim_unop (Unop_struct_get _))) ->
            begin match base.desc with
            | Trm_apps (f', [base'; index], _) ->
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
                  | Trm_apps (_, [base''], _) when is_get_operation base'  -> base''
                  | _ -> base'
                   in
                  let index  = match index.desc with
                  | Trm_apps (_, [index'], _) when is_get_operation index -> index'
                  | _ -> index
                   in
                  begin match base'.typ with
                  | Some {typ_desc = Typ_array({typ_desc = Typ_constr (y, _, _);_}, _);_} when (typconstr_has_name y struct_name) ->
                     let base' = aux base' in
                     let index = aux index in
                     (* keep outer annotations *)
                     trm_apps ~annot:t.annot ?loc:t.loc ?typ:t.typ f' [trm_apps f [base']; index]
                  | Some {typ_desc = Typ_constr (y, _, _); _} when (typconstr_has_name y struct_name) ->
                     (* x might appear both in index and in base' *)
                     let base' = aux base' in
                     let index = aux index in
                     (* keep outer annotations *)
                     trm_apps ~annot:t.annot ?loc:t.loc ?typ:t.typ f' [trm_apps f [base']; index]
                  | Some {typ_desc = Typ_ptr {inner_typ = {typ_desc = Typ_constr (y, _, _); _}; _};_}
                       when (typconstr_has_name y struct_name) ->
                     (* x might appear both in index and in base' *)
                     let base' = aux base' in
                     let index = aux index in
                     (* keep outer annotations *)
                     trm_apps ~annot:t.annot ?loc:t.loc ?typ:t.typ f' [trm_apps f [base']; index]
                  | _ -> trm_map aux t
                  end

               | _ -> trm_map aux t
               end
            | _ -> trm_map aux t
            end
         | _ -> trm_map aux t
         end
      | _ -> trm_map aux t
      end
      (* LATER: arthur: test the use of exceptions for structuring this code *)

    | Trm_typedef td when td.typdef_tconstr = struct_name ->
      begin match td.typdef_body with
      | Typdef_record rf ->
        let rf = List.map (fun (rf1, rf_annot) ->
          match rf1 with
          | Record_field_member (lb, ty) -> ((Record_field_member (lb, typ_array ty (Trm (trm_var sz)))), rf_annot)
          | Record_field_method _ -> (Record_field_method (trm_map aux t), rf_annot)
        ) rf in
        trm_typedef ~annot:t.annot {td with typdef_body = Typdef_record rf}

      | Typdef_alias ty ->
          begin match ty.typ_desc with
          | Typ_array (a, _)->
            begin match a.typ_desc with
            | Typ_constr (sn, _, _) when (typconstr_has_name sn struct_name)-> trm_typedef {td with typdef_body  = Typdef_alias a}

            | _ -> trm_map aux t
            end

          | _ -> trm_map aux t
          end
      | _ -> trm_fail t "aos_to_soa_aux: expected a typedef struct"
      end
    | Trm_typedef td ->
        begin match td.typdef_body with
        | Typdef_alias ty ->
          begin match ty.typ_desc with
          | Typ_array (a, _)->
            begin match a.typ_desc with
            | Typ_constr (sn, _, _) when (typconstr_has_name sn struct_name) -> trm_typedef {td with typdef_body  = Typdef_alias a}

            | _ -> trm_map aux t
            end

          | _ -> trm_map aux t
          end
        | _ -> trm_map aux t
        end

    | Trm_let (vk, (n, dx), init) ->
       begin match dx.typ_desc with
       | Typ_ptr {inner_typ = ty;_} ->
        begin match ty.typ_desc with
        | Typ_array (a, _) ->
          begin match a.typ_desc with
          | Typ_constr (sn,_, _) when (typconstr_has_name sn struct_name) ->
            trm_let_mut ~annot:t.annot (n, a) (trm_uninitialized ?loc:init.loc ())
          | _ -> trm_map aux t
          end
        | _ -> trm_map aux t
        end
       | _ -> trm_map aux t
       end
    | _ -> trm_map aux t
  in aux t

(* [aos_to_soa tv sz t p]: applies [aos_to_soa_aux] at trm [t] with path [p]. *)
let aos_to_soa (tv : typvar) (sz : var): Target.Transfo.local =
  Target.apply_on_path(aos_to_soa_aux tv sz)

(* [set_explicit_aux t]: transoform an initialized array declaration into a list of write operations
      for each one of its cells
    [t] - ast of the array declaration. *)
let set_explicit_aux (t : trm) : trm =
  match t.desc with
  | Trm_let (vk, (x, tx), init) ->
    let init = match get_init_val init with
    | Some init -> init
    | None -> trm_fail t "set_explicit_aux: could not get the initialization trms for the targeted array declaration" in
    begin match init.desc with
    | Trm_array tl ->
      let array_set_list =
      List.mapi ( fun i t1 ->
        trm_set (trm_apps (trm_binop (Binop_array_access)) [trm_var_get x;trm_int i]) t1
      ) (Mlist.to_list tl) in
      let new_decl = trm_let_mut ~annot:t.annot (x, (get_inner_ptr_type tx)) (trm_uninitialized ?loc:init.loc ()) in
      trm_seq_nobrace_nomarks ([new_decl] @ array_set_list)
    | _ -> trm_fail init "set_explicit_aux: expected an array initialization"
    end
  | _ -> trm_fail t "set_explicit_aux: expected an array declaration"


(* [set_explicit t p]: applies [set_explicit_aux] at trm [t] with path [p]. *)
let set_explicit : Target.Transfo.local =
  Target.apply_on_path (set_explicit_aux )
