open Prelude
open Target
open Matrix_trm

(* [intro_calloc tg]: expects the target [tg] to point at  a call to funciton alloc then it will
    replace this call with a call to CALLOC. *)
let%transfo intro_calloc (tg : target) : unit =
  Target.apply_at_target_paths Matrix_core.intro_calloc_aux tg

(* [intro_malloc tg]: expects the target [tg] to point at a call to the function MALLOC,
      then it will replace this call with a call to MALLOC. *)
let%transfo intro_malloc (tg : target) : unit =
  Target.apply_at_target_paths Matrix_core.intro_malloc_aux tg

(* [intro_mindex dim tg]. expects the target [tg] to point at an array access
    then it will replace that access to let say index i with an access at
    MINDEX (dim,i). *)
let%transfo intro_mindex (dim : trm) (tg : target) : unit =
  Target.apply_at_target_paths (Matrix_core.intro_mindex_aux dim) tg

(* [reorder_dims order tg]: expects the target [tg] to point at a call to ALLOC or MINDEX functions,
      then it will reorder their args based on [order], where [order] is a list of indices which the
      current args should follow. *)
let%transfo reorder_dims ?(rotate_n : int = 0) ?(order : int list = []) (tg : target) : unit =
  Target.apply_at_target_paths (Matrix_core.reorder_dims_aux rotate_n order) tg

(* [insert_alloc_dim new_dim]: expects the target [tg] to point at call to ALLOC functions, then it will
      add a new arg at the begining of the list of args in the targeted call. *)
let%transfo insert_alloc_dim (new_dim : trm) (tg : target) : unit =
  Target.apply_at_target_paths (Matrix_core.insert_alloc_dim_aux new_dim) tg

(* [insert_access_dim new_dim new_index tg]: expects the target [tg] to point at an array access, then it will
    add two new args([new_dim] and [new_index]) in the call to MINDEX function inside that array access. *)

let%transfo insert_access_dim_index (new_dim : trm) (new_index : trm) (tg : target) : unit =
  Target.apply_at_target_paths (Matrix_core.insert_access_dim_index_aux new_dim new_index) tg

(* [biject fun_name tg]: expectes the target [tg] to point at a function call, then it replaces the name
     of the called function with [fun_name]. *)
let%transfo biject (fun_name : var) (tg : target) : unit =
  Expr.replace_fun fun_name tg

(* TODO: implement using local_name_tile to avoid duplication *)
(* [local_name ~mark var into tg]: expects the target to point at an instruction that contains
      an occurrence of [var] then it will define a matrix [into] whose dimensions will be the same
      as the one of [var]. Then we copy the contents of the matrix [var] into [into] and finally we
      free up the memory. *)
let%transfo local_name ?(my_mark : mark = no_mark) ?(indices : (string list) = []) ?(alloc_instr : target option) (v : var) ~(into : string) ?(local_ops : local_ops = Local_arith (Lit_int 0, Binop_add)) (tg : target) : unit =
  let remove = (my_mark = no_mark) in
  let get_alloc_type_and_trms (t : trm) (tg1 : target) : typ * (trms * trm * bool) =
    let var_type = begin match t.desc with
      | Trm_let (_, (_, ty), _) -> get_inner_ptr_type ty
      | Trm_apps (_, [lhs; _rhs], _) when is_set_operation t ->
        begin match lhs.typ with
        | Some ty -> ty
        | None -> trm_fail t (Printf.sprintf "Matrix_basic.get_alloc_type_and_trms: couldn't findd the type of variable %s\n'" (var_to_string v))
        end
      | _ -> trm_fail t (Printf.sprintf "Matrix_basic.get_alloc_type_and_trms: couldn't findd the type of variable %s, alloc_instr
          target doesn't point to a write operation or a variable declaration \n'" (var_to_string v))
      end in
      let alloc_trms = begin match Target.get_trm_at (tg1 @ [Target.cFun ~regexp:true ".ALLOC."]) with
        | Some at ->
          begin match Matrix_trm.alloc_inv at with
          | Some (dims, sz, zero_init) -> (dims, sz, zero_init)
          | _ -> trm_fail t "Matrix_basic.get_alloc_type_and_trms: couldn't get the dimensions and the size of the matrix"
          end
        | None -> failwith "Matrix_basic.get_alloc_type_and_trms: couldn't get the dimensions and the size of the matrix"
        end in (var_type, alloc_trms)
    in
  Nobrace_transfo.remove_after ~remove (fun _ ->
    Target.(apply_on_targets (fun t p ->
      let seq_p, _ = Internal.isolate_last_dir_in_seq p in
      let seq_tg = target_of_path seq_p in
      let var_target = cOr [[cVarDef v.name]; [cWriteVar v.name]] in
      begin match alloc_instr with
      | Some tg1 ->
        begin match get_trm_at tg1 with
        | Some t1 ->
          let var_type, alloc_trms = get_alloc_type_and_trms t1 tg1 in
          if not remove then Nobrace.enter();
          Target.apply_on_path (Matrix_core.local_name_aux my_mark v into alloc_trms var_type indices local_ops) t p
        | None -> failwith "Matrix_basical_name: alloc_instr target does not match to any ast node"
        end
      | None ->
        begin match get_trm_at (seq_tg @ [var_target]) with
        | Some t1 ->
          let tg1 = (seq_tg @ [var_target]) in
          let var_type, alloc_trms = get_alloc_type_and_trms t1 tg1 in
          if not remove then Nobrace.enter();
          Target.apply_on_path (Matrix_core.local_name_aux my_mark v into alloc_trms var_type indices local_ops) t p

        | None -> failwith "Matrix_basical_name: alloc_instr target does not match to any ast node"
        end
      end
    ) tg)
  )

let shift_groups = toplevel_var "shift_groups"

let ghost_shift
  ((range, formula): loop_range list * formula)
  ((shifted_range, shifted_formula): loop_range list * formula)
  (uninit_pre : bool) (uninit_post : bool): trm =
  (* FIXME: this can be explained as a sequence of calls to group_shift* ghosts *)
  let open Resource_formula in
  let before = List.fold_right (fun r f -> formula_group_range r f) range formula in
  let after = List.fold_right (fun r f -> formula_group_range r f) shifted_range shifted_formula in
  let before = if uninit_pre then formula_uninit before else before in
  let after = if uninit_post then formula_uninit after else after in
  Resource_trm.ghost_rewrite before after (trm_var shift_groups)

(** <private> *)
let local_name_tile_on (mark_accesses : mark) (var : var) (nd_range : Matrix_core.nd_range)
  (local_var : string) (dims : trms) (elem_ty : typ) (_size : trm)
  (indices : string list) (uninit_pre : bool) (uninit_post : bool)
  (t : trm) : trm =
  let local_var = Trm.new_var local_var in
  let indices_list = begin match indices with
  | [] -> List.mapi (fun i _ -> new_var ("i" ^ (string_of_int (i + 1)))) dims
  | _ -> List.map new_var indices end
  in
  let indices = List.map trm_var indices_list in
  let nested_loop_range = List.map2 (fun (a, b) index ->
    { index; start = a; direction = DirUp; stop = b; step = Post_inc }
  ) nd_range indices_list in
  let tile_dims = List.map (fun (a, b) -> trm_sub b a) nd_range in
  let tile_indices = List.map2 (fun (offset, _) ind -> trm_sub ind offset) nd_range indices in
  let alloc_instr = Matrix_core.let_alloc_with_ty local_var tile_dims elem_ty in
  let map_indices = List.map (fun (offset, _) -> fun i -> trm_sub i offset) nd_range in
  let new_t = Matrix_core.replace_all_accesses var local_var tile_dims
    map_indices mark_accesses t in
  let access_var = access (trm_var var) dims indices in
  let access_local_var = access (trm_var local_var) tile_dims tile_indices in
  let write_on_local_var = trm_set access_local_var (trm_get access_var) in
  let write_on_var = trm_set access_var (trm_get access_local_var) in
  let var_cell = Resource_formula.(formula_model access_var trm_cell) in
  let local_var_cell = Resource_formula.(formula_model access_local_var trm_cell) in
  let load_for = if uninit_pre
    then trm_seq_nobrace_nomarks []
    else trm_copy (Matrix_core.pointwise_fors
      ~reads:[var_cell] ~writes:[local_var_cell] nested_loop_range write_on_local_var) in
  let unload_for = if uninit_post
    then trm_seq_nobrace_nomarks []
    else trm_copy (Matrix_core.pointwise_fors
      ~reads:[local_var_cell] ~writes:[var_cell] nested_loop_range write_on_var) in
  let free_instr = free tile_dims (trm_var local_var) in
  let alloc_range = List.map2 (fun size index ->
    { index; start = trm_int 0; direction = DirUp; stop = size; step = Post_inc }
  ) tile_dims indices_list in
  let alloc_access = access (trm_var local_var) tile_dims indices in
  let alloc_cell = Resource_formula.(formula_model alloc_access trm_cell) in
  let alloc_range_cell = (alloc_range, alloc_cell) in
  let local_var_range_cell = (nested_loop_range, local_var_cell) in
  let shift_res = ghost_shift alloc_range_cell local_var_range_cell true uninit_pre in
  let unshift_res = ghost_shift local_var_range_cell alloc_range_cell uninit_post true in
  trm_seq_nobrace_nomarks [
    alloc_instr; shift_res; load_for;
    new_t;
    unload_for; unshift_res; free_instr]

(** [local_name_tile ?mark_accesses ?indices ~alloc_instr ?ret_var ~local_var tile tg]
  expects [alloc_instr] to point to an allocation of [var] and [tg] to point to a term [t]
  that contains occurrences of [var], and instead of [t]:
  + defines a matrix [local_var] that will correspond to a [tile] of matrix [var];
  + copies the contents from [var] to [local_var];
  + performs [t] where all accesses to [var] are replaced with accesses to [local_var];
  + copies the contents from [local_var] to [var];
  + frees up the memory of [local_var].

  - [mark_accesses] allows marking the produced [local_var] accesses.
  - [indices] allows providing explicit index variable names for the created for loop.
  - [ret_var] will receive the value [var].
  - [uninit_pre]/[uninit_post] specify whether [var] is uninit before/after [t], eliminating copies.
    For [uninit_pre = false], resource checks will fail if [var] is uninit before [t].
    For [uninit_pre = true], resource checks will fail if [var] is not uninit in [t],
    because [local_var] will be uninit.
    For [uninit_post = false], resource checks will always succeed.
    For [uninit_post = true], we check that [trm_ghost_forget_init ..var..] can be added after [t].

  TODO?
  - what if [alloc_instr] is not available? make it optional, retrieve var, dims, elem_ty, ...
    from MINDEX calls, as done by {! Matrix_stack_copy}
  - stack_alloc / heap alloc
  - check consistent API with {! Variable.local_name}
  - factorize and update {! Matrix_basic.local_name} with no tile
  - factorize with {! Matrix_stack_copy}
  *)
let%transfo local_name_tile ?(mark_accesses : mark = no_mark)
  ?(indices : string list = []) ~(alloc_instr : target) ?(ret_var : var ref = ref dummy_var)
  (* TODO: check [uninit_pre] and [uninit_post] in resources,
     could also be inferred instead of provided *)
  ?(uninit_pre : bool = false) ?(uninit_post : bool = false)
  ~(local_var : string) (tile : Matrix_core.nd_range) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
    Target.iter (fun p -> Marks.with_fresh_mark_on p (fun m ->
      let t1 = Xoption.unsome_or_else (get_trm_at alloc_instr) (fun () ->
        failwith "alloc_instr target does not match to any ast node"
      ) in
      let error = "alloc_instr should target a matrix allocation" in
      let v, dims, elem_ty, size = trm_inv ~error Matrix_core.let_alloc_inv_with_ty t1 in
      ret_var := v;
      Target.apply_at_path (local_name_tile_on
        mark_accesses v tile local_var dims elem_ty size indices uninit_pre uninit_post
      ) p;
      if !Flags.check_validity then begin
        Resources.ensure_computed ();
        let p = resolve_target_exactly_one [cMark m] in
        if uninit_post then begin
          let pred formula =
            Var_set.mem !ret_var (trm_free_vars formula)
          in
          Resources.assert_instr_effects_shadowed ~pred p;
          Trace.justif "local effects on replaced variable are shadowed"
        end;
        (* TODO: is this exactly the same check as for variables? *)
        let t = resolve_path p in
        let t_res_usage = Resources.usage_of_trm t in
        let t_res_before = Resources.(filter_touched t_res_usage (before_trm t)) in
        let t_res_after = Resources.(filter_touched t_res_usage (after_trm t)) in
        let used_vars = Var_set.union (Resource_set.used_vars t_res_before) (Resource_set.used_vars t_res_after) in
        if Var_set.mem !ret_var used_vars then
          trm_fail t "resources still mention replaced variable after transformation"
        else
          Trace.justif "resources do not mention replaced variable after transformation"
      end
    )) tg
  )

(* TODO: Factorize me *)
(* [delocalize_aux dim init_zero acc_in_place acc any_mark labels index]: TODO  *)
let delocalize_aux (dim : trm) (init_zero : bool) (acc_in_place : bool) (acc : string option) (any_mark : mark) (labels : label list) (index : string) (ops : local_ops) (t : trm) : trm =
  let index = new_var index in
  match t.desc with
  | Trm_seq tl ->
    if Mlist.length tl < 5 then trm_fail t "Matrix_core.delocalize_aux: the targeted  sequence does not have the correct shape";
    let add_labels = List.length labels = 3 in
    let decl = Mlist.nth tl 0 in
    begin match decl.desc with
    | Trm_let (_, (local_var, ty), init) ->
      begin match get_init_val init with
      | Some init1 ->
        begin match init1.desc with
         | Trm_apps (_, [alloc_trm], _) ->
          begin match alloc_inv alloc_trm with
          | Some (dims, _, _) ->
              let alloc_arity = List.length dims in
              let new_alloc_trm = Matrix_core.insert_alloc_dim_aux dim alloc_trm in
              let new_decl = trm_let_mut (local_var, (get_inner_ptr_type ty)) (trm_cast (get_inner_ptr_type ty) new_alloc_trm) in
              let snd_instr = Mlist.nth tl 1 in
              begin match trm_fors_inv alloc_arity snd_instr with
              | Some (loop_range, body) ->
                let new_dims = dim :: dims in
                let indices = List.fold_left (fun acc range -> (trm_var range.index) :: acc) [] (List.rev loop_range) in
                let new_indices = (trm_var index) :: indices in
                let new_loop_range = loop_range @ [{ index; start = trm_int 0; direction = DirUp; stop = dim; step = Post_inc }] in
                let tg = [nbAny; cCellAccess ~base:[cVar local_var.name] ()] in
                let set_instr =
                begin match body.desc with
                | Trm_seq tl when Mlist.length tl = 1->
                  Mlist.nth tl 0
                | _ -> body
                end in
                begin match ops with
                | Local_arith (li, op) ->
                  begin match set_inv set_instr with
                  | Some (base, dims, indices, old_var_access) ->
                    let acc, acc_provided = match acc with
                    | Some s ->
                      if s = "" then "s",false else s,true
                    | None -> "s", false
                   in
                  let new_access = access base new_dims new_indices in
                  let init_val = trm_lit li in
                  let init_trm =
                    if init_zero
                      then trm_seq_nomarks [set base new_dims new_indices init_val]
                      else trm_seq_nomarks [
                        set base new_dims((trm_int 0) :: indices) old_var_access;
                        trm_for { index; start = trm_int 1; direction = DirUp; stop = dim; step = Post_inc } (set base new_dims new_indices init_val;)]
                      in

                    let op_fun (l_arg : trm) (r_arg : trm) = trm_prim_compound op l_arg r_arg in
                    let acc_trm  =
                    if acc_in_place
                      then
                      if acc_provided
                        then trm_fail t "Matrix_core.delocalize_aux: if acc_in_place is set to true there is not need to provide an accumulator"
                        else begin
                          trm_seq_nomarks [
                           trm_set (get_operation_arg old_var_access) (trm_get (access (base) new_dims ((trm_int 0) :: indices)));
                           trm_for { index; start = trm_int 1; direction = DirUp; stop = dim; step = Post_inc } ( op_fun (get_operation_arg old_var_access) (trm_get new_access))]
                        end
                      else
                        if not acc_provided then trm_fail t "Matrix_core.delocalize_aux: accumulator should be provided otherwise you need to set the flag ~acc_in_place to false" else
                          let acc_var = new_var acc in
                          (trm_seq_nomarks [
                            trm_let_mut (acc_var, typ_double ()) init_val;
                            trm_for { index; start = trm_int 0; direction = DirUp; stop = dim; step = Post_inc } (trm_seq_nomarks [
                                op_fun (trm_var acc_var) (trm_get new_access)]);
                            trm_set (get_operation_arg old_var_access) (trm_var_get acc_var)]) in
                  let new_fst_instr =
                    if add_labels then begin
                      let label_to_add = List.nth labels 0 in
                        if label_to_add = ""
                        then new_decl
                        else trm_add_label label_to_add (trm_seq_nobrace_nomarks [
                          trm_let_mut (local_var, (get_inner_ptr_type ty)) (trm_uninitialized ());
                          (trm_set (trm_var local_var) ((trm_cast (get_inner_ptr_type ty) new_alloc_trm)))])
                      end
                    else new_decl in

                  let new_snd_instr = if init_zero
                    then trm_fors new_loop_range init_trm
                    else trm_fors loop_range init_trm in

                  let thrd_instr = Mlist.nth tl 2 in
                  let ps2 = Constr.resolve_target tg thrd_instr in
                  let new_thrd_instr =
                    List.fold_left (fun acc p ->
                      apply_on_path (Matrix_core.insert_access_dim_index_aux dim (trm_add_mark any_mark (trm_apps (trm_var (name_to_var "ANY")) [dim]))) acc p
                    ) thrd_instr ps2 in

                  let new_frth_instr =
                    trm_fors loop_range acc_trm in

                  let fifth_instr = Mlist.nth tl 4 in
                  let new_fifth_instr = if add_labels then
                     let label_to_add = List.nth labels 2 in
                     trm_add_label label_to_add fifth_instr
                      else fifth_instr in

                    trm_seq ~annot:t.annot (Mlist.of_list [new_fst_instr; trm_copy new_snd_instr; new_thrd_instr; trm_copy new_frth_instr; new_fifth_instr])
                  | _ -> trm_fail set_instr "Matrix_core.delocalize_aux"
                  end
                | Local_obj (_init_f, _merge_f, free_f) ->

                  let new_fst_instr =
                    if add_labels then begin
                      let label_to_add = List.nth labels 0 in
                        if label_to_add = ""
                        then new_decl
                        else (trm_seq_nobrace_nomarks [
                          trm_let_mut (local_var, (get_inner_ptr_type ty)) (trm_uninitialized ());
                          (trm_set (trm_var local_var) ((trm_cast (get_inner_ptr_type ty) new_alloc_trm)))])
                      end
                    else new_decl in

                  let ps1 = Constr.resolve_target tg body in
                  let new_snd_instr =
                    let updated_mindex =
                    List.fold_left (fun acc p ->
                      apply_on_path (Matrix_core.insert_access_dim_index_aux dim (trm_var index)) acc p
                    ) body ps1 in
                    (* TODO: Implement the case when init_zero = false *)
                    trm_fors new_loop_range updated_mindex in

                  let thrd_instr = Mlist.nth tl 2 in
                  let ps2 = Constr.resolve_target tg thrd_instr in
                  let new_thrd_instr =
                    List.fold_left (fun acc p ->
                      apply_on_path (Matrix_core.insert_access_dim_index_aux dim (trm_add_mark any_mark (trm_apps (trm_var (name_to_var "ANY")) [dim]))) acc p
                    ) thrd_instr ps2 in

                  let frth_instr = Mlist.nth tl 3 in
                  let new_frth_instr = begin match trm_fors_inv alloc_arity frth_instr with
                    | Some (loop_range, body) ->
                      let new_loop_range = loop_range @ [{ index; start = trm_int 0; direction = DirUp; stop = dim; step = Post_inc }] in
                      let ps2 = Constr.resolve_target tg body in
                      let new_body =
                          List.fold_left (fun acc p ->
                        apply_on_path (Matrix_core.insert_access_dim_index_aux dim (trm_var index)) acc p
                      ) body ps2  in
                      trm_fors new_loop_range new_body
                    | _ -> trm_fail t "Matrix_core.delocalize_aux: expected the accumulation loop"
                    end in

                  let fifth_instr = Mlist.nth tl 4 in
                  let new_fifth_instr = begin match trm_fors_inv alloc_arity fifth_instr with
                    | Some (loop_range, body) ->
                      let new_loop_range = loop_range @ [{ index; start = trm_int 0; direction = DirUp; stop = dim; step = Post_inc }] in
                      let ps2 = Constr.resolve_target tg body in
                      let new_body =
                          List.fold_left (fun acc p ->
                        apply_on_path (Matrix_core.insert_access_dim_index_aux dim (trm_var index)) acc p
                      ) body ps2  in
                      trm_fors new_loop_range new_body
                    | _ -> trm_fail t "Matrix_core.delocalize_aux: expected the accumulation loop"
                    end in

                  let sixth_instr = Mlist.nth tl 5 in
                    let final_groups =
                      if List.length labels = 0 then [new_fst_instr; trm_copy new_snd_instr; new_thrd_instr; trm_copy new_frth_instr; trm_copy new_fifth_instr; sixth_instr]
                       else List.mapi ( fun i lb ->
                        let new_subsgroup = if i = 0
                          then trm_seq_nobrace_nomarks [new_fst_instr; new_snd_instr]
                          else if i = 1 then trm_seq_nobrace_nomarks [new_thrd_instr; new_frth_instr]
                          else trm_seq_nobrace_nomarks [new_fifth_instr; sixth_instr]
                          in
                        trm_add_label lb new_subsgroup

                       ) labels
                    in
                  trm_seq ~annot:t.annot (Mlist.of_list final_groups)
                end

              | _ -> trm_fail snd_instr "Matrix_core.delocalize_aux: expected the nested loops where the local matrix initialization is done"
              end
          | _ -> trm_fail init "Matrix_core.delocalize_aux: the local variable should be declared together with its mermory allocation"
          end
         | _ -> trm_fail init1 "Matrix_core.delocalize_aux: couldn't find the cast operation "
        end

      | _ -> trm_fail init "Matrix_core.couldn't get the alloc trms for the target local variable declaration"
      end
    | _ -> trm_fail t "Matrix_core.delocalize_aux: expected the declaration of the local variable"
    end
  |  _ -> trm_fail t "Matrix_core.delocalize_aux: expected sequence which contains the mandatory instructions for applying the delocalize transformation"

(* [delocalize ~init_zero ~acc_in_place ~acc ~dim ~index ~ops] a generalized version of variable_delocalize. *)
let%transfo delocalize ?(init_zero : bool = false) ?(acc_in_place : bool = false) ?(acc : string option) ?(any_mark : mark = no_mark) ?(labels : label list = []) ~(dim: trm) ~(index: string) ~ops:(dl_o : local_ops) (tg : target) : unit =
    Target.apply_at_target_paths (delocalize_aux dim init_zero acc_in_place acc any_mark labels index dl_o) tg

let assert_same_dims (a : trms) (b : trms) : unit =
  (* TODO: need something better for term equality *)
  if not (List.for_all2 Internal.same_trm a b) then begin
    Tools.warn "Matrix_basic: dimensions mismatch";
    Show.trms ~msg:"a" a;
    Show.trms ~msg:"b" b;
  end

(* TODO: check that size and index expressions are pure, otherwise fail *)
let simpl_index_add_on (t : trm) : trm =
  let error = "Matrix_basic.simpl_index_on: expected MINDEX addition" in
  let (a, b) = trm_inv ~error trm_add_inv t in
  let mindex1 = trm_inv ~error mindex_inv a in
  let mindex2 = trm_inv ~error mindex_inv b in
  let ((long_dims, long_idxs), (short_dims, short_idxs)) =
    if (List.length (fst mindex1)) > (List.length (fst mindex2))
    then (mindex1, mindex2) else (mindex2, mindex1)
  in
  let delta_dims = (List.length long_dims) - (List.length short_dims) in
  let trimmed_long_dims = Xlist.drop delta_dims long_dims in
  (* DEBUG
  Printf.printf "delta: %i\n" delta_dims;
  Debug.trms "trimmed long" trimmed_long_dims;
  Debug.trms "short" short_dims;
  *)
  assert_same_dims trimmed_long_dims short_dims;
  let rec compute_idxs (delta : int) (long : trms) (short : trms) : trms =
    if delta > 0 then
      (List.hd long) :: (compute_idxs (delta - 1) (List.tl long) short)
    else match (long, short) with
    | (l :: l_rest, s :: s_rest) ->
      (trm_add l s) :: (compute_idxs 0 l_rest s_rest)
    | ([], []) -> []
    | _ -> assert false
  in
  mindex long_dims (compute_idxs delta_dims long_idxs short_idxs)

(* [simpl_index_add]: simplifies an MINDEX(..) + MINDEX(..) expression,
   into a single MINDEX(..) expression, if the dimensions are compatible:

   MINDEX{N}  (            n1, .., nN,                 i1, .., iN) +
   MINDEX{N+M}(m1, .., mM,     .., m{N+M}, j1, .., jM,     .., j{N+M})
    = [if n{i} = m{i+M}]
   MINDEX{N+M}(m1, .., mM,     .., m{N+M}, j1, .., jM, i1 + j{M+1}, .., iN + j{N+M})

   For correctness, size and index expressions must be pure.
   *)
let%transfo simpl_index_add (tg : target) : unit =
  Resources.justif_correct "arguments are reproducible";
  Trace.tag_simpl_access ();
  Target.apply_at_target_paths simpl_index_add_on tg;
  Scope.infer_var_ids () (* Needed because we generate MINDEX variables by name *)

let simpl_access_of_access_on (t : trm) : trm =
  let error = "Matrix_basic.simpl_access_of_access_on: expected nested array accesses" in
  let (base1, i1) = match array_access_inv t with
  | Some res -> res
  (* FIXME: don't want to deal with this here? *)
  | None -> trm_inv ~error array_get_inv t
  in
  let (base0, i0) = trm_inv ~error array_access_inv base1 in
  array_access base0 (trm_add i0 i1)

(* [simpl_access_of_access]: simplifies &((&p[i0])[i1]) into &p[i0 + i1]

   TODO: should this be in another file?
   *)
let%transfo simpl_access_of_access (tg : target) : unit =
  Trace.justif_always_correct ();
  Trace.tag_simpl_access ();
  Target.apply_at_target_paths simpl_access_of_access_on tg

(* internal *)
let find_occurences_and_add_mindex0 (x : var) (t : trm) : (bool * trm) =
  let found = ref false in
  let rec loop (t : trm) : trm =
    match trm_var_inv t with
    | Some y when x = y ->
      found := true;
      trm_array_access (trm_var y) (mindex [] [])
    | _ -> trm_map loop t
  in
  let res_t = loop t in
  (!found, res_t)

let intro_malloc0_on (mark_alloc : mark) (mark_free : mark) (x : var) (t : trm) : trm = begin
  let instrs = trm_inv
    ~error:"Matrix_basic.intro_malloc0_on: expected sequence"
    trm_seq_inv t
  in
  let decl_info_opt = ref None in
  Mlist.iteri (fun i instr ->
    match trm_let_inv instr with
    | Some (_, y, ty, init) when x = y ->
      if (is_trm_new_uninitialized init) ||
         (is_trm_uninitialized init)
      then begin
        assert (Option.is_none !decl_info_opt);
        decl_info_opt := Some (i, ty);
      end
    | _ -> ()
  ) instrs;
  match !decl_info_opt with
  | Some (decl_index, decl_ty) ->
    let new_decl = trm_add_mark mark_alloc (
      Matrix_core.let_alloc_with_ty x [] (get_inner_ptr_type decl_ty)) in
    let instrs2 = Mlist.replace_at decl_index new_decl instrs in
    let last_use = ref decl_index in
    let instrs3 = Mlist.mapi (fun i instr ->
      if i <= decl_index then
       instr
      else begin
        let (found, instr2) = find_occurences_and_add_mindex0 x instr in
        if found then last_use := i;
        instr2
      end
    ) instrs2 in
    let free_instr = trm_add_mark mark_free (Matrix_trm.free [] (trm_var x)) in
    let instrs4 = Mlist.insert_at (!last_use + 1) free_instr instrs3 in
    trm_seq ~annot:t.annot instrs4
  | None -> trm_fail t "Matrix_basic.intro_malloc0_on: expected unintialized stack allocation"
end

(* [intro_malloc0]: given a target to a sequence with a declaration allocating
   variable [x] on the stack, changes the declaration to use a MALLOC0 heap
   allocation, and adds an instruction to free the memory after all uses of
   [x] in the sequence.

   {
    T* x = new T*;
    ... uses x ...
    ...
   }
   -->
   {
     T* x = MALLOC0(sizeof(T));
     ... uses &x[MINDEX0()] ...
     free(x);
     ...
   }

   LATER: deal with control flow

   *)
let%transfo intro_malloc0
  ?(mark_alloc : mark = no_mark) ?(mark_free : mark = no_mark)
  (x : var) (tg : target) : unit =
  Trace.justif_always_correct ();
  Target.apply_at_target_paths (intro_malloc0_on mark_alloc mark_free x) tg

(** <private>

  f(name)

  --->

  T stack_name[N];
  memcpy(stack_name, &name[MINDEX(...)], sizeof(T[...]));
  f(name)[
    stack_name[iN] /
    name[MINDEX(..., iN)]
  ];
  memcpy(&name[MINDEX(...)], stack_name, sizeof(T[...]));
*)
let stack_copy_on (var : var) (copy_name : string) (copy_dims : int) (t : trm) : trm =
  let ret_dims_and_typ : (trms * typ) option ref = ref None in
  let common_indices_opt : trms option ref = ref None in
  let stack_var = new_var copy_name in
  let new_t = Matrix_core.map_all_accesses var ~ret_dims_and_typ (fun dims indices ->
    let (common_indices, new_indices) = Xlist.split_at copy_dims indices in
    begin match !common_indices_opt with
    | Some ci -> assert (List.for_all2 Internal.same_trm ci common_indices);
    | None -> common_indices_opt := Some common_indices
    end;
    let new_dims = Xlist.take_last copy_dims dims in
    List.fold_left (fun acc i -> Matrix_trm.access acc new_dims [i])
      (trm_var stack_var) new_indices
  ) t in
  let (dims, typ) = Option.get !ret_dims_and_typ in
  let common_indices = Option.get !common_indices_opt in
  let new_dims = Xlist.take_last copy_dims dims in
  (* let array_typ = List.fold_left (fun acc i -> typ_array acc (Trm i)) typ new_dims in *)
  trm_seq_nobrace_nomarks [
    (* TODO: define Matrix_core.stack_alloc, FIXME: new with dims has to be uninit? use different prim? *)
    trm_let Var_immutable (stack_var, typ_const_ptr typ) (trm_new typ ~dims:new_dims (trm_uninitialized ()));
    Matrix_core.memcpy_with_ty
      (trm_var stack_var) [] new_dims
      (trm_var var) common_indices dims
      new_dims typ;
    new_t;
    Matrix_core.memcpy_with_ty
      (trm_var var) common_indices dims
      (trm_var stack_var) [] new_dims
      new_dims typ;
  ]

(** [stack_copy ~var ~copy_var ~copy_dims tg] expects [tg] to points at a term [t]
    that contains occurences of matrix [var], and instead of [t]:
    + defines a matrix [copy_var] that will correspond to the right-most [copy_dims]
      of [var], which are contiguous in memory;
    + copies the contents from [var] to [copy_var] using a memcpy;
    + performs [t] where all accesses to [var] are replaced with accesses to [copy_var];
    + copies the contents from [copy_var] to [var] using a memcpy;

  TODO:
  - [uninit_pre]/[uninit_post] as in {! Matrix.local_name_tile}

  This should be equivalent to using {! Matrix.local_name_tile}, converting its heap
  allocation to a stack allocation with something like {! Matrix.to_array},
  and converting copy loops to batch memory copies.
  *)
let%transfo stack_copy ~(var : var) ~(copy_var : string) ~(copy_dims : int) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () ->
    Target.iter (fun p -> Marks.with_fresh_mark_on p (fun m ->
      Target.apply_at_path (stack_copy_on var copy_var copy_dims) p;
      if !Flags.check_validity then begin
        Resources.ensure_computed ();
        (* TODO: is this exactly the same check as for Variable.local_name and Matrix.local_name? *)
        let t = get_trm_at_exn [cMark m] in
        let t_res_usage = Resources.usage_of_trm t in
        let t_res_before = Resources.(filter_touched t_res_usage (before_trm t)) in
        let t_res_after = Resources.(filter_touched t_res_usage (after_trm t)) in
        let used_vars = Var_set.union (Resource_set.used_vars t_res_before) (Resource_set.used_vars t_res_after) in
        if Var_set.mem var used_vars then
          trm_fail t "resources still mention replaced variable after transformation"
        else
          Trace.justif "resources do not mention replaced variable after transformation"
      end
    )) tg)

let elim_mindex_on (t : trm) : trm =
  let (dims, idxs) = trm_inv
    ~error:"Matrix_basic.elim_mindex_on: expected MINDEX expression"
    mindex_inv t in
  let rec generate_index (acc : trm) (dims : trms) (idxs : trms) : trm =
    match (dims, idxs) with
    | (d :: dr, i :: ir) ->
      let new_acc = trm_add acc (List.fold_left trm_mul i dr) in
      generate_index new_acc dr ir
    | _ -> acc
  in
  generate_index (trm_int 0) dims idxs

(* [elim_mindex] expects target [tg] to point at a call to MINDEX,
  and replaces it with the flattened index computation.

   Equivalent to:
   Rewrite.equiv_at ~ctx:true "int d1, d2, i1, i2; ==> MINDEX2(d1, d2, i1, i2) == (i1 * d2 + i2)" tg
   Rewrite.equiv_at ~ctx:true "int d1, d2, d3, i1, i2, i3; ==> MINDEX3(d1, d2, d3, i1, i2, i3) == (i1 * d2 * d3 + i2 * d3 + i3)" tg
   [...]
   *)
let%transfo elim_mindex (tg : target) : unit =
  Trace.justif "correct if size and index expressions are pure (TODO: check)";
  Target.apply_at_target_paths elim_mindex_on tg

let storage_folding_on (var : var) (dim : int) (n : trm) (t : trm) : trm =
  let new_dims = ref [] in
  let rec update_accesses_and_alloc (t : trm) : trm =
    match Matrix_trm.access_inv t with
    | Some (f, dims, indices) ->
      begin match trm_var_inv f with
      | Some v when v = var -> begin
        new_dims := Xlist.update_nth dim (fun _ -> n) dims;
        let new_indices = Xlist.update_nth dim (fun i -> trm_mod i n) indices in
        Matrix_trm.access ~annot:t.annot f !new_dims new_indices
        end
      | _ -> trm_map update_accesses_and_alloc t
      end
    | None ->
      begin match Matrix_core.let_alloc_inv_with_ty t with
      | Some (v, dims, etyp, size) when v = var ->
        let new_dims = Xlist.update_nth dim (fun _ -> n) dims in
        trm_let ~annot:t.annot Var_immutable (v, typ_const_ptr etyp) (Matrix_core.alloc_with_ty new_dims etyp)
      | _ ->
        begin match trm_var_inv t with
        | Some n when n = var ->
          trm_fail t "Matrix_basic.storage_folding_on: variable access is not covered"
        | _ ->
          begin match Matrix_trm.free_inv t with
          | Some freed ->
            begin match trm_var_inv freed with
            | Some n when n = var ->
              Matrix_trm.free !new_dims freed
            | _ -> trm_map update_accesses_and_alloc t
            end
          | None -> trm_map update_accesses_and_alloc t
          end
        end
      end
  in
  update_accesses_and_alloc t

type storage_folding_kind =
| ModuloIndices
| RotateVariables

let storage_folding_kind_to_string = function
| ModuloIndices -> "ModuloIndices"
| RotateVariables -> "RotateVariables"

(* [storage_folding] expects target [tg] to point at a sequence defining matrix
   [var], and folds the [dim]-th dimension so that every index [i] into this matrix dimension is mapped to index [i % n].

   assumes that [i >= 0].
   *)
let%transfo storage_folding ~(var : var) ~(dim : int) ~(size : trm)
  ?(kind : storage_folding_kind = ModuloIndices) (tg : target) : unit =
  assert(kind = ModuloIndices);
  Target.apply_at_target_paths (storage_folding_on var dim size) tg

(* TODO: redundant code with storage folding *)
let delete_on (var : var) (t : trm) : trm =
  let rec update_accesses_and_alloc (t : trm) : trm =
    (* TODO: use let_alloc_inv_with_ty *)
    match trm_let_inv t with
    | Some (_kind, v, vtyp, init) when v = var ->
      (* TODO: deal with CALLOC *)
      assert (Option.is_some (Matrix_core.alloc_inv_with_ty init));
      trm_seq_nobrace_nomarks []
    | _ ->
      begin match trm_var_inv t with
      | Some n when n = var ->
        trm_fail t "Matrix_basic.delete_on: matrix should not be used anymore"
      | _ ->
        let is_free_var = begin match Matrix_trm.free_inv t with
        | Some freed ->
          begin match trm_var_inv freed with
          | Some n -> n = var
          | None -> false
          end
        | None -> false
        end in
        if is_free_var then trm_seq_nobrace_nomarks []
        else trm_map update_accesses_and_alloc t
      end
  in
  update_accesses_and_alloc t

(* [delete] expects target [tg] to poitn to a sequence defining matrix [var], and deletes it.
  Both allocation and de-allocation instructions are deleted.
  Checks that [var] is not used anywhere.
   *)
let%transfo delete ~(var : var) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () ->
    Target.apply_at_target_paths (delete_on var) tg)

(* [read_last_write]: expects the target [tg] to pint at a matrix read operation, and replaces it with the value that was last written to this matrix index. The [write] target must correspond to this last write.
  For correctness, if [V] was written at index [i], reading [V[j/i]] should be equivalent to reading at index [j].
   *)
let%transfo read_last_write ~(write : target) (tg : target) : unit =
  Scope.infer_var_ids (); (* FIXME: This should be done by previous transfo instead *)
  let write_trm = match Target.get_trm_at write with
  | Some wt -> wt
  | None -> failwith "Matrix_basic.read_least_write: write target not found"
  in
  let (wr_base, wr_dims, wr_indices, wr_value) = trm_inv
    ~error:"Matrix_basic.read_last_write: targeted matrix write operation is not supported"
    Matrix_trm.set_inv write_trm
  in
  Target.apply_at_target_paths (fun t ->
    let (rd_base, rd_dims, rd_indices) = trm_inv
      ~error:"Matrix_basic.read_last_write: targeted matrix read operation is not supported"
      Matrix_trm.get_inv t
    in
    assert_same_dims wr_dims rd_dims;
    if not (Internal.same_trm wr_base rd_base) then
      trm_fail t "Matrix_basic.read_last_write: array base mistmach";
    let rd_value = List.fold_left (fun value (wr_i, rd_i) ->
      begin match trm_var_inv wr_i with
      | Some wr_i_var ->
        trm_subst_var wr_i_var rd_i value
      | None ->
        let error = "Matrix_basic.read_last_write: expected write index to be a variable, or to be the same as the read index" in
        if (Internal.same_trm wr_i rd_i) then value
        else trm_fail wr_i error
      end
    ) wr_value (List.combine wr_indices rd_indices)
    in
    rd_value
  ) tg
