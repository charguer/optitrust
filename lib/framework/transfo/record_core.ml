open Prelude
open Target

(** [set_explicit_on t]: transforms an assigment into a list of field assignments,
     [t] - ast of the assignment. *)
let set_explicit_on (t : trm) : trm =
  (* TODO: needs refactoring *)
  match t.desc with
  | Trm_apps (f, [lt; rt], _) ->
    (* Temporary hack for overloaded set operator *)
    let lt = begin match trm_prim_inv f with
      | Some (Prim_binop Binop_set)
      | Some (Prim_overloaded_op (Prim_binop Binop_set)) -> lt
      | _ -> trm_fail f "expected set operator"
      end
     in
    let tid = match Option.bind rt.typ typ_constr_inv, Option.bind (Option.bind lt.typ typ_ptr_inv) typ_constr_inv with
      | None, Some var | Some var, None -> var
      | Some var_r, Some var_l ->
        if not (var_eq var_r var_l) then trm_fail t (sprintf "different types in an assignment ('%s' vs '%s')" (var_to_string var_l) (var_to_string var_r));
        var_r
      | None, None ->
        trm_fail t "explicit assignment cannot operate on unknown types"
    in
    let struct_def =
      match Internal.typvar_to_typedef tid with
      | Some td -> td
      | _ -> trm_fail t (sprintf "could not get the declaration of typedef for %s" (var_to_string tid))
    in
    let field_list = Internal.get_field_list struct_def in
    let check_pure = if !Flags.check_validity then (fun name x ->
      if Resources.trm_is_pure x then Trace.justif (sprintf "duplicated %s is pure" name)
    ) else (fun name x ->
      ()
    ) in
    (* already checked by set contract:
       check_pure "lhs" lt; *)
    if !Flags.check_validity then Trace.justif "duplicated terms are pure";
    (* clause is Reads or Writes *)
    let unfold_cells clause_locs =
      let open Resource_formula in
      let open Resource_contract in
      if !Flags.check_validity then begin
        let make_admitted pure linear1 linear2 =
          Resource_trm.ghost_admitted {
            pre = Resource_set.make ~pure ~linear:linear1 ();
            post = Resource_set.make ~linear:linear2 () }
        in
        let make_one_pair (clause, loc) =
          match clause with
          | Reads ->
            let per_field_admits = List.map (fun (sf, ty) ->
              let with_fresh_fracs () =
                let frac_var, frac_ghost = new_frac () in
                let folded_res =
                  formula_read_only ~frac:(trm_var frac_var) (formula_model loc trm_cell)
                in
                let unfolded_res =
                  formula_read_only ~frac:(trm_var frac_var)
                    (formula_model (trm_struct_access ~typ:(typ_ptr ty) loc sf) trm_cell)
                in
                let wand = formula_wand unfolded_res folded_res in
                let folded_linear = [(new_anon_hyp (), folded_res)] in
                let unfolded_linear = [
                  (new_anon_hyp (), wand);
                  (new_anon_hyp (), unfolded_res)] in
                (frac_ghost, folded_linear, unfolded_linear)
              in
              let (p, f, u) = with_fresh_fracs () in
              let (p2, f2, u2) = with_fresh_fracs () in
              (make_admitted [p] f u, make_admitted [p2] u2 f2)
            ) field_list in
            let (per_field_unfolds, per_field_folds) = List.split per_field_admits in
            (trm_seq_nobrace_nomarks per_field_unfolds,
             trm_seq_nobrace_nomarks per_field_folds)
          | Writes ->
            let folded_linear = [(
              new_anon_hyp (), formula_model loc trm_cell
            )] in
            let unfolded_linear = List.map (fun (sf, ty) ->
              (new_anon_hyp (), formula_model
                (trm_struct_access ~typ:(typ_ptr ty) loc sf)
                trm_cell
              )
            ) field_list in
            let make_uninit = List.map resource_item_uninit in
            (make_admitted [] (make_uninit folded_linear) (make_uninit unfolded_linear),
             make_admitted [] unfolded_linear folded_linear)
          | _ -> failwith "unexpected unfold clause"
        in
        let pairs = List.map make_one_pair clause_locs in
        List.split pairs
      end else ([], [])
    in
    (* TODO: handle unfolded resource overlap between rhs and lhs *)
    let ((before, after), set_one) = begin match rt.desc with
    | Trm_apps (f1, [rt1], _) when is_get_operation rt ->
      (* lt = get(rt1) --> lt.f = get(rt1.f) *)
      (* NOTE: already checked by ghosts
        check_pure "rhs" rt; *)
      let set_one i (sf, ty) =
        trm_set (trm_struct_access ~typ:(typ_ptr ty) lt sf)
          (trm_get (trm_struct_access ~typ:(typ_ptr ty) rt1 sf))
          (* {rt with desc = Trm_apps (f1, [trm_struct_access ~typ:ty rt1 sf], []); typ = Some ty} *)
      in
      (unfold_cells [Writes, lt; Reads, rt1], set_one)
    | Trm_record st ->
      (* lt = { .f = v; .. } --> lt.f = v *)
      let st = List.split_pairs_snd (Mlist.to_list st) in
      let set_one i (sf, ty) =
        trm_set (trm_struct_access ~typ:(typ_ptr ty) lt sf) (List.nth st i)
      in
      (unfold_cells [Writes,lt], set_one)
    | _ ->  (* other cases are included here *)
      (* lt = rt --> lt.f = rt.f *)
      check_pure "rhs" rt;
      let set_one i (sf, ty) =
        trm_set (trm_struct_access ~typ:(typ_ptr ty) lt sf) (trm_struct_get ~typ:ty rt sf)
      in
      (unfold_cells [Writes,lt], set_one)
    end in
    trm_seq_helper ~braces:false [ TrmList before; TrmList (List.mapi set_one field_list); TrmList after]
  | _ -> trm_fail t "expected a set operation"

(** [set_implicit t]: transform a sequence with a list of explicit field assignments into a single assignment,
      [t] - ast of the sequence containing the assignments. *)
let set_implicit_on (t: trm) : trm =
  match t.desc with
  | Trm_seq tl ->
     let rhs_trms = Mlist.fold_left ( fun acc instr ->
      match instr.desc with
      | Trm_apps (_, [_;rhs], _) ->
        begin match rhs.desc with
        | Trm_apps(f', [rt], _)  ->
          begin match f'.desc with
          | Trm_prim (Prim_unop Unop_get) ->
            begin match rt.desc with
              | Trm_apps(f'',[rt], _) ->
               begin match f''.desc with
               | Trm_prim (Prim_unop (Unop_struct_access _))
               | Trm_prim (Prim_unop (Unop_struct_get _))->
                  [trm_get rt]
               | _ -> trm_fail f' "Record_core.set_implicit_on: expected a struct acces on the right hand side of the assignment"
               end
              | _ -> trm_fail f' "Record_core.set_implicit_on: expected a trm_apps"
            end
            | Trm_prim (Prim_unop (Unop_struct_access _))
            | Trm_prim (Prim_unop (Unop_struct_get _))->
                  [rt]
            | _ -> trm_fail f' "Record_core.set_implicit_on: expected a struct acces on the right hand side of the assignment"
           end
          | _ -> acc @ [rhs]
          end
      | _ -> trm_fail t "Record_core.set_implicit_on: expected a set operation"
    ) [] tl in
    let first_instruction = Mlist.nth tl 0 in
    begin match first_instruction.desc with
    | Trm_apps(f,[lhs;_], _) ->
          begin match f.desc with
          | Trm_prim (Prim_binop Binop_set) ->
            let lt = begin match lhs.desc with
            | Trm_apps(f', [lt], _) ->
              begin match f'.desc with
              | Trm_prim (Prim_unop (Unop_struct_access _))
              | Trm_prim (Prim_unop (Unop_struct_get _))-> lt
              | _ -> trm_fail f' "Record_core.set_implicit_on: expected a struct access on the left hand side of the assignment"
              end
            | _ -> trm_fail lhs "Record_core.set_implicit_on: expected a struct access"
            end
            in
            begin match rhs_trms with
            | [rhs1] -> trm_pass_labels t (trm_set lt rhs1)
            | _ ->
              let rhs_trms = List.map (fun t1 -> (None, t1)) rhs_trms in
              trm_pass_labels t (trm_set lt (trm_record (Mlist.of_list rhs_trms)))
            end
          | _ -> trm_fail f "Record_core.set_implicit_on: expected an assignment instruction"
          end
      | _ -> trm_fail t "Record_core.set_implicit_on: expected a sequence with all explicit assignments"

    end
  | _ -> trm_fail t "Record_core.set_implicit_on: sequence which contains the set instructions was not matched"

(** [contains_field_access f t]: checks if [t] contains an access on field [f] *)
let contains_field_access (f : field) (t : trm) : bool =
  let rec aux (t : trm) : bool =
   match t.desc with
   | Trm_apps (f', tl, _) ->
      begin match f'.desc with
      | Trm_prim (Prim_unop (Unop_struct_access f1)) -> f = f1
      | Trm_prim (Prim_unop (Unop_struct_get f1)) -> f = f1
      | _ -> List.fold_left (fun acc t1 -> acc || aux t1) false tl
      end
   | _ -> false
  in aux t

(** [inline_struct_accesses x t]: changes all the occurrences of the struct accesses to a field into a field,
      [x] - the name of the field for which the transformation is applied,
      [t] - ast node located in the same level as the stract declaration or deeper. *)
let inline_struct_accesses (x : field) (t : trm) : trm =
  let rec aux (outer_field : string) (t : trm) : trm =
    match t.desc with
    | Trm_apps (f, base, _) ->
      begin match f.desc with
      | Trm_prim (Prim_unop (Unop_struct_access z)) ->
        begin match base with
        | [base'] ->
          if contains_field_access x base'
            then aux z base'
            else if outer_field <> "" then
              let updated_field = Convention.name_app z outer_field in
              trm_struct_access base' updated_field
            else trm_map (aux "") t
        | _ -> trm_fail f "Record_core.inline_struct_access: suspicious struct access"
        end
      | Trm_prim (Prim_unop (Unop_struct_get z)) ->
        begin match base with
        | [base'] ->
          if contains_field_access x base'
            then aux z base'
            else if outer_field <> "" then
              let updated_field = Convention.name_app z outer_field in
              trm_struct_get base' updated_field
            else trm_map (aux "") t
        | _ -> trm_fail f "Record_core.inline_struct_access: suspicious struct access"
        end
      | _ -> trm_map (aux outer_field) t
      end
    | _ -> trm_map (aux outer_field) t

   in aux "" t

(** [inline_struct_initialization struct_name field_list field_index t]: changes all struct in struct initializations,
      [struct_name] - the type of the struct that is being inlined,
      [field_list] - a list of fields from the original type of the struct,
      [field_index] - index of the field in the outer struct,
      [t] - ast node located in the same level as the main struct declaration or deeper. *)
let inline_struct_initialization (struct_name : typvar) (field_list : field list) (field_index : int) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    (* Searching for struct intialization lists of type typedef struct {} struct_name *)
    | Trm_record term_list ->
      begin match t.typ with
      | Some ty ->
        let ty = get_inner_const_type ty in
        Pattern.pattern_match ty [
          Pattern.(typ_constr (var_eq struct_name)) (fun () ->
            let lfront, (_,trm_to_change) , lback = Mlist.get_item_and_its_relatives field_index term_list in
            begin match trm_to_change.desc with
            | Trm_record sl ->
              let new_term_list = Mlist.merge_list [lfront; sl; lback] in
              trm_record ~annot:t.annot ?typ:t.typ new_term_list

            | Trm_apps (_, [{desc = Trm_var p;_} as v], _) when is_get_operation trm_to_change ->
              let sl = List.map (fun f -> (None, trm_get (trm_struct_access (trm_var ?typ:v.typ p) f))) field_list in
              let new_term_list = Mlist.merge_list [lfront; Mlist.of_list sl; lback] in
              trm_record ~annot:t.annot ?typ:t.typ new_term_list

            | Trm_var p ->
              let sl = List.map (fun f -> (None, trm_struct_get (trm_var ?typ:t.typ p) f)) field_list in
              let new_term_list = Mlist.merge_list [lfront; Mlist.of_list sl; lback] in
              trm_record ~annot:t.annot ?typ:t.typ new_term_list

            | _ -> trm_fail t "Record_core.inline_struct_initialization: struct intialization list is not compatible with definition"
            end
          );
          Pattern.__ (fun () -> trm_map aux t)
        ]
      | _ -> trm_fail t "Record_core.inline_struct_initialization: couldn't find the type of the struct intitialization type, try reparsing first"
        end
    | _ -> trm_map aux t
  in aux t

(** [reveal_field_at field_to_reveal index t]: reveals field [field_to_reveal] on its typedef struct definition,
     update all the initializations and accesses according to this change,
      [field_to_reveal] - field that is going to be revealed,
      [index] - index of the struct declaration inside the sequence it belongs to,
      [t] - trm corresponding to a typedef struct definition. *)
let reveal_field_at (field_to_reveal : field) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let td_name = ref dummy_var in
    let f_list = ref [] in
    let field_index = ref 0 in
    let f_update (t : trm) : trm =
      match t.desc with
      | Trm_typedef td ->
        begin match td.typedef_body with
        | Typedef_record field_list ->
          field_index := Internal.get_field_index t field_to_reveal field_list;
          let (field_to_reveal_rf,_),field_list  = List.extract_element field_list !field_index in
          let field_type = get_member_type t field_to_reveal_rf in
          let tyvar =
            Pattern.pattern_match field_type [
              Pattern.(typ_array (typ_constr !__) __ ^| typ_constr !__) (fun tv () -> tv);
              Pattern.__ (fun () -> trm_fail t "Record_core.reveal_field_aux: expected a type constr")
            ]
          in
          let struct_def = match Internal.typvar_to_typedef tyvar with
            | Some td -> td
            | _ -> trm_fail t "Record_core.reveal_field_aux: could not get the declaration of typedef"
          in

          let inner_type_field_list = match struct_def.typedef_body with
          | Typedef_record rfl -> rfl
          | _ -> trm_fail t "Record_core.reveal_field_aux: the field wanted to inline should also be of struct type"
          in

          let inner_type_field_list = Internal.rename_record_fields (fun f ->
             Convention.name_app field_to_reveal f) inner_type_field_list in

          let typ_update (ty : typ) : typ =
            match typ_array_inv field_type with
              | Some (_, size) -> typ_array ty ?size
              | _ -> ty
            in
          let inner_type_field_list = Internal.update_record_fields_type typ_update inner_type_field_list in

          let field_list = List.insert_sublist_at !field_index inner_type_field_list field_list in

          td_name := td.typedef_name;
          f_list := fst (List.split (Internal.get_field_list struct_def));

          let new_typedef = {td with typedef_body = Typedef_record field_list} in
          trm_replace (Trm_typedef new_typedef) t

        | _ -> trm_fail t "Record_core.reveal_field_aux: expected a struct definition"
        end
      | _ -> trm_fail t "Record_core.reveal_field_aux: expected a target to a type definition"
      in
    let f_update_further (t : trm) : trm =
      let t = inline_struct_accesses field_to_reveal t in
      inline_struct_initialization !td_name !f_list !field_index t
      in
    let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> trm_fail t "Record_core.reveal_field_aux: expected the surrounding sequence"

(** [fields_order]: the order should be provided as argument to the transformation [reorder_fields]. *)
type fields_order =
  | Move_before of (field * field list)
  | Move_after of (field * field list)
  | Reorder_all of field list

(** [compute_bijection order fl]: based on the [order] given, computes the bijection
    of the indices after applying that order. *)
let compute_bijection (order : fields_order) (fl : (field * int) list) : int list =
  match order with
  | Move_before (field, fields_to_move) ->
    let filtered_fl = List.filter (fun (f, _) -> not (List.mem f fields_to_move)) fl in
    let fields_to_move_ind = List.map (fun f -> match List.assoc_opt f fl with
      | Some ind -> (f, ind)
      | None -> failwith "Record_core.compute_bijection: catastrophic error ."
    ) fields_to_move in
    let upd_fl =
    List.fold_left (fun acc (f, ind) ->
      if f = field then fields_to_move_ind @ (f, ind) :: acc
        else (f, ind) :: acc
    ) [] (List.rev filtered_fl)
      in
    List.map snd upd_fl
  | Move_after (field, fields_to_move) ->
    let filtered_fl = List.filter (fun (f, _) -> not (List.mem f fields_to_move)) fl in
    let fields_to_move_ind = List.map (fun f -> match List.assoc_opt f fl with
      | Some ind -> (f, ind)
      | None -> failwith "Record_core.compute_bijection: catastrophic error ."
    ) fields_to_move in
    let upd_fl =
    List.fold_left (fun acc (f, ind) ->
      if f = field then (f, ind) :: fields_to_move_ind @acc
        else (f, ind) :: acc
    ) [] (List.rev filtered_fl)
      in
    List.map snd upd_fl
  | Reorder_all order ->
    if List.length order <> List.length fl then failwith "Record_core.compute_bijection: Reorder all should contain all the fields.";
    List.map (fun f -> match List.assoc_opt f fl with
      | Some ind -> ind
      | None -> failwith "Record_core:compute_bijection: couldn't find field %s." f
    ) order

(** [reorder_fields_at order index t]: reorders the fields of the struct [t] based on [order],
     [order] - order based on which the fields will be reordered,
     [t] - ast of the typedef Record. *)
let reorder_fields_at (order : fields_order) (index : int) (t : trm) : trm =
  let error = "Record_core.reorder_fields_aux: expected the surrouding sequence of the targeted declaration." in
  let tl = trm_inv ~error trm_seq_inv t in
  let bij = ref [] in
  let struct_name = ref dummy_var in
  let f_update (t : trm) : trm =
    match t.desc with
    | Trm_typedef td ->
      struct_name := td.typedef_name;
      begin match td.typedef_body with
      | Typedef_record rfl ->
        let rfl_str_rep = List.mapi (fun i (rf, _) ->
          match rf with
          | Record_field_member (lb, _) -> (lb, i)
          | Record_field_method t1 ->
            begin match trm_typedef_inv t1 with
            | Some td -> (td.typedef_name.name, i)
            | _ -> trm_fail t "Record_core.reorder_fields_at: unkown method definition."
            end
        ) rfl in
        bij := compute_bijection order rfl_str_rep;
        let new_rfl = List.reorder !bij rfl in
        trm_alter ~desc:(Trm_typedef {td with typedef_body = Typedef_record new_rfl}) t

      | _ -> trm_fail t "Record_core.reorder_fields_at: expected a target to a record type definition."

      end
    | _ -> trm_fail t "Record_core.reorder_fields_at: expected a target pointing to a typedef."
    in
  let f_update_further (t : trm) : trm =
    let rec aux (t : trm) : trm =
      match t.desc with
      | Trm_record mlt ->
        let struct_name = !struct_name in
        Pattern.pattern_match t.typ [
          Pattern.(some (typ_constr (var_eq struct_name))) (fun () ->
            let lt = Mlist.to_list mlt in
            let reordered_lt = List.reorder !bij lt in
            trm_alter ~desc:(Trm_record (Mlist.of_list reordered_lt)) t
          );
          Pattern.__ (fun () -> trm_map aux t)
        ]
      | _ -> trm_map aux t
      in
    aux t
   in
  let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
  trm_replace (Trm_seq new_tl) t

(** <internal> *)
let to_variables_update (var : var) (is_ref : bool) (fields : (field * typ * var) list) (t : trm) : trm =
  let aux_resource_items =
    let open Resource_formula in
    List.concat_map (fun (h, r) ->
      let (mode, inner_r) = formula_mode_inv r in
      Pattern.pattern_match inner_r [
        Pattern.(formula_model (trm_var (var_eq var)) (trm_var (var_eq var_cell))) (fun () ->
          List.map (fun (f, t, v) ->
            (new_anon_hyp (), formula_map_under_mode (fun _ -> formula_model (trm_var v) (trm_var (var_cell))) r)
          ) fields
        );
        Pattern.__ (fun () -> [(h, r)])
      ]
    )
  in
  let aux_resource_set res =
    { res with
      pure = aux_resource_items res.pure;
      linear = aux_resource_items res.linear }
  in
  let aux_fun_contract contract =
    { pre = aux_resource_set contract.pre;
      post = aux_resource_set contract.post }
  in
  let rec aux (t : trm) : trm =
    (* if var.name = "fieldAtPos" && Var_set.mem var (trm_free_vars t) then
      Show.trm ~style:Style.(internal_ast_only_desc ()) ~msg:"fieldAtPos update" t; *)
    Pattern.pattern_match t [
      Pattern.(
        (trm_struct_access !__ (trm_var (var_eq var)))
        ^| (trm_struct_get !__ (trm_var (var_eq var)))
      ) (fun field () ->
        match List.find_opt (fun (f, t, v) -> field = f) fields with
        | Some (f, t, v) -> trm_var v
        | None -> raise Pattern.Next
      );
      Pattern.(trm_struct_get !__ (trm_get (trm_var (var_eq var)))) (fun field () ->
        match List.find_opt (fun (f, t, v) -> field = f) fields with
        | Some (f, t, v) -> trm_var_get v
        | None -> raise Pattern.Next
      );
      Pattern.(trm_var (var_eq var)) (fun () ->
        Pattern.when_ (not is_ref);
        trm_record (Mlist.of_list (List.map (fun (f, t, v) ->
          None, trm_var v
        ) fields))
      );
      Pattern.(trm_get (trm_var (var_eq var))) (fun () ->
        Pattern.when_ is_ref;
        trm_record (Mlist.of_list (List.map (fun (f, t, v) ->
          None, trm_var_get v
        ) fields))
      );
      (* TODO: also do other contracts *)
      Pattern.(trm_for !__ !__ !__) (fun range body spec () ->
        let contract = { spec with
          invariant = aux_resource_set spec.invariant;
          parallel_reads = aux_resource_items spec.parallel_reads;
          iter_contract = aux_fun_contract spec.iter_contract;
        } in
        trm_map aux (trm_for ~annot:t.annot ~contract range body)
      );
      Pattern.(trm_fun_with_contract !__ !__ !__) (fun args body contract () ->
        let contract = aux_fun_contract contract in
        trm_map aux (trm_fun ~annot:t.annot ~contract:(FunSpecContract contract) args None body)
      );
      Pattern.__ (fun () -> trm_map aux t)
    ]
   in aux t

(** [to_variables_at index t]: changes a variable declaration of type typedef struct into a list
      of variable declarations with types inherited from the fields of the underlying type,
      [index] - index of the declaration inside the sequence it belongs to,
      [t] - ast of the surrounding sequence of the variable declarations. *)
let to_variables_at (index : int) (t : trm) : trm =
  let error = "expected a surrounding sequence" in
  let tl = trm_inv ~error trm_seq_inv t in
  let fields = ref [] in
  let var = ref dummy_var in
  let is_ref = ref false in
  let f_update (t : trm) : trm =
    let error = "expected a variable declaration" in
    let (x, tx, init) = trm_inv ~error trm_let_inv t in
    var := x;
    let typvar = Pattern.pattern_match (get_inner_ptr_type tx) [
        Pattern.(typ_constr !__) (fun v () -> v);
        Pattern.__ (fun () -> trm_fail t "expected a struct type")
      ]
    in
    let struct_def = match Internal.typvar_to_typedef typvar with
    | Some td -> td
    | _ -> trm_fail t "could not get the declaration of typedef"
    in
    let field_list = Internal.get_field_list struct_def in
    let init' = match trm_ref_inv init with
    | Some (_, v) -> is_ref := true; v
    | _ -> is_ref := false; init
    in
    let init_list =
      if is_trm_uninitialized init' then None else begin
        let error = "expected a struct initialization" in
        Some (trm_inv ~error trm_record_inv init')
      end
    in
    let var_decls = List.mapi (fun i (sf, ty) ->
      let field_var = new_var (Convention.name_app x.name sf) in
      fields := !fields @ [sf, ty, field_var];
      begin match init_list with
      | None -> trm_let_maybemut !is_ref (field_var, ty) (trm_uninitialized ty)
      | Some inits -> trm_let_maybemut !is_ref (field_var, ty) (snd (Mlist.nth inits i))
      end
    ) field_list in
    trm_seq_nobrace_nomarks var_decls
  in
  let f_update_further (t : trm) : trm = to_variables_update !var !is_ref !fields t in
  let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
  trm_seq ~annot:t.annot new_tl

(* TODO: merge with Variable.Rename *)
(** [Rename]: a module used for renaming the struct fields. *)
module Rename = struct
  type t = string -> string
  let add_prefix (s : string) : t =
    fun str -> s ^ str

  let only_for (pattern : string) : t -> t =
    fun tr s ->
      if Tools.pattern_matches pattern s then tr s else s
end

(** [rename]: instantiation of module [Rename]. *)
type rename = Rename.t

(** [rename_struct_accesses struct_name renam t]: renames all struct accesses based on [rename],
      [struct_name] - the constructed type whose fields are going to be renamed,
      [rename] - a type used to rename the struct fields,
      [t] - any node in the same level as the struct declaration.*)
let rename_struct_accesses (struct_name : typvar) (rename : rename) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps (f, [base], _) ->
      begin match f.desc with
      | Trm_prim (Prim_unop (Unop_struct_access y)) ->
        Pattern.pattern_match base.typ [
          Pattern.(some (typ_constr (var_eq struct_name))) (fun () ->
            trm_apps ~annot:t.annot ?typ:t.typ {f with desc = Trm_prim (Prim_unop (Unop_struct_access (rename y)))} [base]
          );
          Pattern.__ (fun () -> trm_map aux t)
        ]
      | Trm_prim (Prim_unop (Unop_struct_get y)) ->
        Pattern.pattern_match base.typ [
          Pattern.(some (typ_constr (var_eq struct_name))) (fun () ->
            trm_apps ~annot:t.annot ?typ:t.typ {f with desc = Trm_prim (Prim_unop (Unop_struct_get (rename y)))} [base]
          );
          Pattern.__ (fun () -> trm_map aux t)
        ]
      | _ -> trm_map aux t
      end
    | Trm_apps ({desc = Trm_var qf; _} as f, args, ghost_args) when trm_has_cstyle Method_call t ->
      let member_base = fst (List.uncons args) in
      Pattern.pattern_match (get_operation_arg member_base).typ [
        Pattern.(some (typ_constr (var_eq struct_name))) (fun () ->
          let renamed_var = { namespaces = qf.namespaces; name = rename qf.name; id = qf.id } in
          trm_apps ~annot:t.annot ?typ:t.typ ~ghost_args {f with desc = Trm_var renamed_var} args
        );
        Pattern.__ (fun () -> trm_map aux t)
      ]
    | _ -> trm_map aux t
   in aux t

(** [rename_fields_at index rename t]: renames struct fields in the typedef struct definitions,
      [index] - the index of the struct declaration in the sequence [t],
      [rename] - a type used to rename the fields,
      [t] - the ast of the sequence which contains the struct declaration. *)
let rename_fields_at (index : int) (rename : rename) (t : trm) : trm =
  let error = "Record_core.rename_fields_aux: expected the sequence which contains the typedef declaration." in
  let tl = trm_inv ~error trm_seq_inv t in
  let struct_name = ref dummy_var in
  let f_update (t : trm) : trm =
    match t.desc with
    | Trm_typedef ({typedef_name = name; typedef_body = Typedef_record rfl;_} as td) ->
      struct_name := name;
      let rfl = Internal.rename_record_fields rename rfl in
      trm_typedef ~annot:t.annot {td with typedef_body = Typedef_record rfl}
   | _ -> trm_fail t "Record_core.reanme_fields_aux: expected a typedef declaration"
   in
  let f_update_further (t : trm) : trm =
    rename_struct_accesses !struct_name rename t
   in
  let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
  trm_seq ~annot:t.annot new_tl

(** [update_fields_type_aux pattern ty t]: changes the current type for all the struct fields,
      that are matched with [pattern] and whose type can be changed by [typ_update].
      [pattern] - regular expression to match struct fields,
      [typ_update] - function that modifies only specific types,
      [t] - the ast of the typedef definition. *)
let update_fields_type_on (pattern : string) (typ_update : typ -> typ) (t : trm) : trm =
  match t.desc with
  | Trm_typedef ({typedef_body = Typedef_record rfl;_}  as td) ->
    (* LATER: FIX ME! *)
    (* let update_type ty = typ_map typ_update ty in *)
    let update_type (ty_to_update : typ) : typ = ty_to_update in

    (* let replace_type (s : string) (ty1 : typ) : typ =
      if Tools.pattern_matches pattern s then (update_type ty1)  else ty1 in
     *)
    let rfl = Internal.update_record_fields_type ~pattern update_type rfl in
    (* let new_fl = List.map (fun (x, ty2) -> (x, replace_type x ty2)) fl in *)
    trm_typedef ~annot:t.annot {td with typedef_body = Typedef_record rfl}
  | _ -> trm_fail t "Record_core.reanme_fields_aux: expected a typedef declaration"


(** [simpl_proj_on t]: transforms all expression of the form {1, 2, 3}.f into the trm it projects to,
      [t] - ast of the node whose descendants can contain struct initialization list projections. *)
let simpl_proj_on (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps (f, [struct_list], _) ->
      begin match trm_prim_inv f with
      | Some (Prim_unop (Unop_struct_get x)) | Some (Prim_unop (Unop_struct_access x))->
        begin match struct_list.desc with
        | Trm_record tl ->
          begin match Option.bind struct_list.typ typ_constr_inv with
          | Some tvar ->
            let struct_def = match Internal.typvar_to_typedef tvar with
            | Some td -> td
            | _ -> trm_fail struct_list "Record_core.simpl_proj_aux: couldn't retrieve the the struct declaration" in
            let field_list = Internal.get_field_list struct_def in
            let field_vars = fst (List.split field_list) in
            begin match List.index_of x field_vars  with
            | Some i -> snd (Mlist.nth tl i)
            | _ -> t
            end
          | None -> t
          end
        | _ -> trm_map aux t
        end
      | _ -> trm_map aux t
      end
    | _ -> trm_map aux t
   in aux t

(** [Struct_modif]: a module for defining struct modifications. *)
module Struct_modif = struct
  (* Fields of a struct *)

  type fields = record_fields
  (* type fields = (label * typ) list *)

  let fields_identity :  fields -> fields =
    Fun.id

  (** [recmodif] is the type of a function such as [f_get],
     which is meant to be called as [f_get aux t], where
     [aux] is the function for recursively processing subterms. *)
  type recmodif = (trm->trm)

  (** Arguments for [struct_modif]:
     - [f_fields] is for modifying struct fields
     - [f_get] is for [get(access(base,f))]
     - [f_set] is for [set(access(base, f), rhs)]
     - [f_struct_get] is for [struct_get(base, f)]
     - [f_access] is for [struct_access(base, f)]
     - [f_alloc] is for [trm_record ls]; the function is provided with the
       old list and the new list of fields, with names and types. *)
  type arg = {
    f_fields : fields -> fields;
    f_get: recmodif -> trm -> trm;
    f_set: recmodif -> trm -> trm;
    f_struct_get: recmodif -> trm -> trm;
    f_access: recmodif -> trm -> trm;
    f_alloc: (fields*fields) -> recmodif -> trm -> trm;
  }

  let arg_must_not_happen : recmodif -> trm -> trm =
    (fun _ _ -> assert false)

  let arg_identity : recmodif -> trm -> trm =
    (fun _aux t -> t)

  let reuse_annot_of (tsrc : trm) (t : trm) : trm =
    { t with  annot = tsrc.annot}

end

(** [modif_accesses struct_name arg t]: modify struct accesses,
    [old_and_new_fields] - used to replace some specific fields,
    [struct_name] - used for checking the type of the struct access,
    [arg] - see Struct_modif module,
    [t] - trm corresponding to the surrounding sequence of the targeted typedef. *)
let modif_accesses (old_and_new_fields : Struct_modif.fields * Struct_modif.fields) (struct_name : typvar) (arg : Struct_modif.arg) (t : trm) : trm =
  let is_target_typ base =
    match base.typ with
    | Some typ -> is_typ_named struct_name typ
    | None -> false
  in
  let rec aux (t : trm) : trm =
    let default () = trm_map aux t in

    match set_struct_access_inv t with
    | Some (base, _field, _rhs) -> (* LATER: use when clause? *)
            if is_target_typ base
              then arg.f_set aux t
              else default()
    | None ->
      begin match get_struct_access_inv t with
      | Some (base, _field) ->
        if is_target_typ base
          then arg.f_get aux t
          else default()
      | None ->
        begin match struct_access_inv t with
        | Some (base, _field) ->
          if is_target_typ base
            then arg.f_access aux t
            else default()
        | None ->
          begin match struct_get_inv t with
          | Some (base, _field) ->
            if is_target_typ base
              then arg.f_struct_get aux t
              else default()
          | None ->
            begin match struct_init_inv t with
            | Some sl ->
              if is_target_typ t
                then arg.f_alloc old_and_new_fields aux t
                else default()
            | None -> default ()
            end
          end
        end
      end
    in aux t

(** [struct_modif_at new_fields f_get f_set use_annot_of index t],
     [arg] - Struct_modif type,
     [index] - index of the typedef on its surrounding sequence,
     [t] - ast of the main sequence containing the typedef definition. *)
let struct_modif_at (arg : Struct_modif.arg) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let tdef = begin match Mlist.nth_opt tl index with
    | Some t1 -> t1
    | None -> assert false end in
    begin match tdef.desc with
    | Trm_typedef td ->
      begin match td.typedef_body with
      | Typedef_record old_fields ->
         let struct_name = td.typedef_name in
         let new_fields = arg.f_fields old_fields in
         let new_typdef = {td with typedef_body = Typedef_record new_fields} in
         let new_td = trm_typedef ~annot:tdef.annot new_typdef in
         let f_update = fun t -> new_td in
         let f_update_further = fun t -> modif_accesses (old_fields, new_fields) struct_name arg t in
         let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
         trm_replace (Trm_seq new_tl) t
      | _ -> trm_fail tdef "Record_core.Record_core.struct_modif: expected a struct definition"
      end
    | _ -> trm_fail tdef "Record_core.Record_core.struct_modif: expected a target to a typedef struct definition"
    end
  | _ -> trm_fail t "Record_core.Record_core.struct_modif: exepcted the surrounding sequence of the typedef "

(** [change_field_access_kind_on acc_kind f t]: changes the access_kind for field [f] to [acc_kind] of class or struct [t]. *)
let change_field_access_kind_on (acc_kind : record_field_annot) (f : field) (t : trm) : trm =
  match t.desc with
  | Trm_typedef td ->
    begin match td.typedef_body with
    | Typedef_record rfs ->
      let new_rfs = List.map (fun (rf, rf_ann) ->
        (* if f is given as the default string then all the fields access kind will be changed. *)
        if f = ""
          then (rf, acc_kind)
          else
            let rf_name_opt = Internal.get_field_name rf in
            begin match rf_name_opt with
            | Some n when n = f -> (rf, acc_kind)
            | _ -> (rf, rf_ann)
            end
        ) rfs in
        let new_td = {td with typedef_body = Typedef_record new_rfs} in
        trm_alter ~desc:(Trm_typedef new_td) t
    | _ -> trm_fail t "Record_core.change_field_access_kind_aux: expected a target to a structured typedef."
    end
  | _ -> trm_fail t "Record_core.change_field_access_kind_aux: expected a targetd to a typedef."


(** [method_to_const_on method_name t]: converts the [method_name] method to a a const one,
    if the targeted method is already const than this transformation does nothing.
    [method_name] - the name of the method that's going to be converted.*)
let method_to_const_on (method_name : var) (t : trm) : trm =
  match t.desc with
  | Trm_typedef td ->
    begin match td.typedef_body with
    | Typedef_record rfl ->
        let upd_rfl = List.map (fun (rf, rf_ann) ->
            match rf with
            | Record_field_method t1  ->
              if is_class_constructor t1
                then (rf, rf_ann)
                else if method_name = dummy_var then (Record_field_method (trm_add_cstyle Const_method t1), rf_ann)
                else
                  failwith "unimplemented, #var-id"
                  (*
                  begin match decl_name t1 with
                  | Some td when method_name = td.typdef_tconstr ->
                    if trm_has_cstyle Const_method t1
                      then (rf, rf_ann)(* begin Printf.printf ("Nothing to change, method %s is already const." method_name); (rf, rf_ann) end *)
                      else
                        let t1 = trm_add_cstyle Const_method t1 in
                        (Record_field_method t1, rf_ann)
                  | _ -> (rf, rf_ann)
                  end *)
            | _ -> (rf, rf_ann)
        ) rfl in

        trm_replace (Trm_typedef {td with typedef_body = Typedef_record upd_rfl}) t
    | _ ->  trm_fail t "Record_core.method_to_const_aux: expected a target to a typedef record definition."
    end
  | _ -> trm_fail t "Record_core.method_to_const_aux: expected a target to a record definition."
