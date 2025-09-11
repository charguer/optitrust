open Prelude
open Target
open Matrix_trm

(** [reorder_dims order tg]: expects the target [tg] to point at a call to MALLOC or MINDEX functions,
      then it will reorder their args based on [order], where [order] is a list of indices which the
      current args should follow. *)
let%transfo reorder_dims ~(base:trm) ?(rotate_n : int = 0) ?(order : int list = []) (tg : target) : unit =
  Target.apply_at_target_paths (Matrix_core.reorder_dims_aux ~base rotate_n order) tg

(** [insert_alloc_dim new_dim]: expects the target [tg] to point at call to ALLOC functions, then it will
      add a new arg at the begining of the list of args in the targeted call. *)
let%transfo insert_alloc_dim (new_dim : trm) (tg : target) : unit =
  Target.apply_at_target_paths (Matrix_core.insert_alloc_dim_aux new_dim) tg

(** [insert_access_dim new_dim new_index tg]: expects the target [tg] to point at an array access, then it will
    add two new args([new_dim] and [new_index]) in the call to MINDEX function inside that array access. *)

let%transfo insert_access_dim_index (new_dim : trm) (new_index : trm) (tg : target) : unit =
  Target.apply_at_target_paths (Matrix_core.insert_access_dim_index_aux new_dim new_index) tg

(** [biject fun_name tg]: expectes the target [tg] to point at a function call, then it replaces the name
     of the called function with [fun_name]. *)
let%transfo biject (fun_name : var) (tg : target) : unit =
  Expr.replace_fun fun_name tg


(** <private>
  returns (ranges, matrix_ptr, mindex_dims, mindex_indices)
  *)
let rec formula_mindex_group_inv (f : formula) : ((formula * var) list * trm * trm list * trm list) option =
  let open Resource_formula in
  Pattern.pattern_match_opt f [
    Pattern.(formula_group !__ !__ !__)
      (fun idx range inner_formula () ->
        match formula_mindex_group_inv inner_formula with
        | Some (ranges, matrix_ptr, mindex_dims, mindex_indices) ->
          ((range, idx) :: ranges, matrix_ptr, mindex_dims, mindex_indices)
        | None -> raise Pattern.Failed
      );
    Pattern.(formula_any_cell !__) (fun location () ->
      match Matrix_trm.access_inv location with
      | Some (matrix, mindex_dims, mindex_indices) -> ([], matrix, mindex_dims, mindex_indices)
      | None -> raise Pattern.Failed
    );
  ]

(* TODO: implement using local_name_tile to avoid duplication *)
(** [local_name ~mark var into tg]: expects the target to point at an instruction that contains
      an occurrence of [var] then it will define a matrix [into] whose dimensions will be the same
      as the one of [var]. Then we copy the contents of the matrix [var] into [into] and finally we
      free up the memory. *)
let%transfo local_name
  ?(my_mark : mark = no_mark)
  ?(indices : (string list) = [])
  ?(alloc_instr : target option) (* TODO: this should be supported at non-basic level *)
  ?(type_and_dims : (typ * trms) option)
  (v : var) ~(into : string)
  ?(uninit_pre : bool = false) ?(uninit_post : bool = false)
  ?(local_ops : local_ops = Local_arith (Lit_int (typ_int, 0), Binop_add))
  (tg : target) : unit =
  let remove = (my_mark = no_mark) in
  let get_type_and_dims (t : trm) (tg1 : target) : typ * trms =
    let var_type = begin match t.desc with
      | Trm_let ((_, ty), _) -> get_inner_ptr_type ty
      | Trm_apps (_, [lhs; _rhs], _, _) when is_set_operation t ->
        begin match lhs.typ with
        | Some ty -> ty
        | None -> trm_fail t (Printf.sprintf "Matrix_basic.get_alloc_type_and_trms: couldn't findd the type of variable %s\n'" (var_to_string v))
        end
      | _ -> trm_fail t (Printf.sprintf "Matrix_basic.get_alloc_type_and_trms: couldn't findd the type of variable %s, alloc_instr
          target doesn't point to a write operation or a variable declaration \n'" (var_to_string v))
      end in
      let dims = begin match Target.get_trm_at (tg1 @ [Target.cNew ()]) with
        | Some at ->
          begin match Matrix_trm.alloc_inv at with
          | Some (ty, dims, _) -> dims
          | _ -> trm_fail t "Matrix_basic.get_alloc_type_and_trms: couldn't get the dimensions and the size of the matrix"
          end
        | None -> failwith "Matrix_basic.get_alloc_type_and_trms: couldn't get the dimensions and the size of the matrix"
        end in (var_type, dims)
    in
  Nobrace_transfo.remove_after ~remove (fun _ ->
    Target.iter (fun p ->
      let seq_p, _ = Internal.isolate_last_dir_in_seq p in
      let seq_tg = target_of_path seq_p in
      let (elem_type, dims) =
        match type_and_dims with
        | Some stuff -> stuff
        | None -> begin match alloc_instr with
          | Some tg1 ->
            begin match get_trm_at tg1 with
            | Some t1 -> get_type_and_dims t1 tg1
            | None -> failwith "Matrix_basical_name: alloc_instr target does not match to any ast node"
            end
          | None ->
            let var_target = cOr [[cVarDef v.name]; [cWriteVar v.name]] in
            begin match get_trm_at (seq_tg @ [var_target]) with
            | Some t1 ->
              let tg1 = (seq_tg @ [var_target]) in
              get_type_and_dims t1 tg1
            | None -> failwith "Matrix_basical_name: alloc_instr target does not match to any ast node"
            end
        end
      in
      if not remove then Nobrace.enter();
      Target.apply_at_path (Matrix_core.local_name_aux my_mark v into dims elem_type indices local_ops) p
    ) tg
  )

let shift_groups = toplevel_var "shift_groups"

let ghost_shift
  ((range, formula): loop_range list * formula)
  ((shifted_range, shifted_formula): loop_range list * formula)
  (uninit_pre : bool) (uninit_post : bool): trm =
  if !Flags.check_validity then (* FIXME: need more precise flag? *)
    (* FIXME: this can be explained as a sequence of calls to group_shift* ghosts *)
    let open Resource_formula in
    let before = List.fold_right (fun r f -> formula_group_range r f) range formula in
    let after = List.fold_right (fun r f -> formula_group_range r f) shifted_range shifted_formula in
    let before = if uninit_pre then formula_uninit before else before in
    let after = if uninit_post then formula_uninit after else after in
    Resource_trm.ghost_admitted_rewrite before after (trm_var shift_groups)
  else
    trm_seq_nobrace_nomarks []

(** <private> *)
let local_name_tile_on (mark_dims : mark)
  (mark_accesses : mark)
  (mark_alloc : mark) (mark_load : mark) (mark_unload : mark)
  (var : var) (nd_range : Matrix_core.nd_range)
  (local_var : string) (dims : trms) (elem_ty : typ option)
  (indices : string list) (uninit_pre : bool) (uninit_post : bool)
  (t : trm) : trm =
  let local_var = new_var local_var in
  let indices_list = begin match indices with
  | [] -> List.mapi (fun i _ -> new_var ("i" ^ (string_of_int (i + 1)))) dims
  | _ -> List.map new_var indices end
  in
  let indices = List.map trm_var indices_list in
  let nested_loop_range = List.map2 (fun (a, b) index ->
    { index; start = a; direction = DirUp; stop = b; step = trm_step_one () }
  ) nd_range indices_list in
  let needs_to_replace_accesses = ref false in
  List.iter2 (fun (a, b) dim ->
    if not (is_trm_int 0 a) || not (Trm_unify.are_same_trm b dim)
    then needs_to_replace_accesses := true;
  ) nd_range dims;
  let tile_dims =
    if !needs_to_replace_accesses
    then List.map (fun (a, b) -> trm_add_mark mark_dims (trm_sub_int b a)) nd_range
    else dims
  in
  let tile_indices =
    if !needs_to_replace_accesses
    then List.map2 (fun (offset, _) ind -> trm_sub_int ind offset) nd_range indices
    else indices
  in
  let (new_t, elem_ty) = if !needs_to_replace_accesses then begin
    let map_indices = List.map (fun (offset, _) -> fun i -> trm_sub_int i offset) nd_range in
    let ret_dims_and_typ = ref (Option.map (fun t -> (dims, t)) elem_ty) in
    let new_t = Matrix_core.replace_all_accesses var local_var tile_dims
      ~ret_dims_and_typ map_indices mark_accesses t in
    let elem_ty = snd (Option.unsome ~error:"expected elem_ty" !ret_dims_and_typ) in
    (new_t, elem_ty)
  end else begin
    (trm_subst_var var (trm_var local_var) t,
     Option.unsome ~error:"expected elem_ty" elem_ty)
  end in
  let alloc_instr = trm_add_mark mark_alloc
    (Matrix_core.let_alloc local_var elem_ty tile_dims) in
  let ptr_ty = typ_ptr elem_ty in
  let var_t = trm_var ~typ:ptr_ty var in
  let local_var_t = trm_var ~typ:ptr_ty local_var in
  let access_var = access var_t dims indices in
  let access_local_var = access local_var_t tile_dims tile_indices in
  let write_on_local_var = trm_set access_local_var (trm_get access_var) in
  let write_on_var = trm_set access_var (trm_get access_local_var) in
  let var_cell = Resource_formula.(formula_cell access_var) in
  let local_var_cell = Resource_formula.(formula_cell access_local_var) in
  let load_for = if uninit_pre
    then trm_seq_nobrace_nomarks []
    else trm_add_mark mark_load (trm_copy (Matrix_core.pointwise_fors
      ~reads:[var_cell] ~writes:[local_var_cell] nested_loop_range write_on_local_var)) in
  let unload_for = if uninit_post
    then trm_seq_nobrace_nomarks []
    else trm_add_mark mark_unload (trm_copy (Matrix_core.pointwise_fors
      ~reads:[local_var_cell] ~writes:[var_cell] nested_loop_range write_on_var)) in
  let free_instr = free local_var_t in
  let alloc_range = List.map2 (fun size index ->
    { index; start = trm_int 0; direction = DirUp; stop = size; step = trm_step_one () }
  ) tile_dims indices_list in
  let alloc_access = access local_var_t tile_dims indices in
  let alloc_cell = Resource_formula.(formula_cell alloc_access) in
  let alloc_range_cell = (alloc_range, alloc_cell) in
  let local_var_range_cell = (nested_loop_range, local_var_cell) in
  let shift_res = ghost_shift alloc_range_cell local_var_range_cell true true in
  let unshift_res = ghost_shift local_var_range_cell alloc_range_cell true true in
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
    from MINDEX calls, as done by {! Matrix.stack_copy}
  - stack_alloc / heap alloc
  - check consistent API with {! Variable.local_name}
  - factorize and update {! Matrix_basic.local_name} with no tile
  - factorize with {! Matrix.stack_copy}
  *)
let%transfo local_name_tile
  ?(mark_dims : mark = no_mark)
  ?(mark_accesses : mark = no_mark)
  ?(mark_alloc : mark = no_mark)
  ?(mark_load : mark = no_mark)
  ?(mark_unload : mark = no_mark)
  ?(indices : string list = [])
  ?(alloc_instr : target option) (* if alloc_instr, return ret_var *)
  ?(ret_var : var ref = ref dummy_var) (* otherwise, input ret_var *)
  ?(elem_ty : typ option)
  (* TODO: check [uninit_pre] and [uninit_post] in resources,
     could also be inferred instead of provided *)
  ?(uninit_pre : bool = false) ?(uninit_post : bool = false)
  ~(local_var : string)
  ?(tile : Matrix_core.nd_range option)
  (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
    Target.iter (fun p -> Marks.with_fresh_mark_on p (fun m ->
      let tile_dims_typ = ref None in
      if !Flags.check_validity then begin
        (* find groups of mindex resource over !ret_var in context *)
        Resources.ensure_computed ();
        let var = !ret_var in
        if var = dummy_var then failwith "expected target matrix variable to be provided";
        let res = Resources.before_trm (get_trm_at_exn (target_of_path p)) in
        let ranges_dims_indices_typ = ref None in
        let process_linear r =
          let Resource_formula.{ formula = r2 } = Resource_formula.formula_read_only_inv_all r in
          match formula_mindex_group_inv r2 with
          | Some (ranges, matrix_ptr, dims, indices) ->
            (* DEBUG: Printf.printf "formula: %s\n" Resource_computation.(formula_to_string r2); *)
            Pattern.pattern_match matrix_ptr [
              Pattern.(trm_specific_var var) (fun () ->
                if Option.is_some !ranges_dims_indices_typ
                then trm_fail r (sprintf "found multiple resources with %s" (var_to_string var))
                else ranges_dims_indices_typ := Some (ranges, dims, indices,
                  Option.bind matrix_ptr.typ typ_ptr_inv)
              );
              Pattern.__ (fun () -> ())
            ]
          | None -> ()
        in
        List.iter (fun (h, r) -> process_linear r) res.linear;
        let (ranges, dims, indices, typ) = Option.unsome ~error:"expected appropriate mindex formula in resource context" !ranges_dims_indices_typ in
        List.iter2 (fun (_, ri) i ->
          Pattern.pattern_match i [
            Pattern.(trm_specific_var ri) (fun () -> ());
            Pattern.__ (fun () -> trm_fail i "groups and MINDEX indices don't match")
          ]
        ) ranges indices;
        let tiles = List.map (fun (r, _) ->
          Pattern.pattern_match r [
            Resource_formula.Pattern.(formula_range !__ !__ (trm_int (eq 1))) (fun a b () ->
              (a, b));
            Pattern.__ (fun () -> trm_fail r "expected range with step 1")
          ]
        ) ranges in
        tile_dims_typ := Some (tiles, dims, typ);
      end else begin
        let t1 = Option.unsome_or_else (get_trm_at (Option.unsome ~error:"expected alloc_instr" alloc_instr)) (fun () ->
          failwith "alloc_instr target does not match to any ast node"
        ) in
        let error = "alloc_instr should target a matrix allocation" in
        let v, elem_ty, dims, _ = trm_inv ~error Matrix_core.let_alloc_inv t1 in
        ret_var := v;
        tile_dims_typ := Some (Option.unsome ~error:"expected tile argument" tile, dims, Some elem_ty)
      end;
      let (tile, dims, collected_elem_ty) = Option.unsome !tile_dims_typ in
      let elem_ty = Option.or_ elem_ty collected_elem_ty in
      Target.apply_at_path (local_name_tile_on
        mark_dims mark_accesses mark_alloc mark_load mark_unload !ret_var tile local_var dims elem_ty indices uninit_pre uninit_post
      ) p;
      if !Flags.check_validity then begin
        Resources.ensure_computed ();
        let p = resolve_target_exactly_one [cMark m] in
        if uninit_post then begin
          let pred formula =
            is_free_var_in_trm !ret_var formula
          in
          Resources.assert_instr_effects_shadowed ~pred p;
          Trace.justif "local effects on replaced variable are shadowed"
        end;
        (* TODO: is this exactly the same check as for variables? *)
        let t = resolve_path p in
        let t_res_usage = Resources.usage_of_trm t in
        let t_res_before = Resource_set.filter_touched t_res_usage (Resources.before_trm t) in
        let t_res_after = Resource_set.filter_touched t_res_usage (Resources.after_trm t) in
        let used_vars = Var_set.union (Resource_set.used_vars t_res_before) (Resource_set.used_vars t_res_after) in
        if Var_set.mem !ret_var used_vars then
          trm_fail t "resources still mention replaced variable after transformation"
        else
          Trace.justif "resources do not mention replaced variable after transformation"
      end
    )) tg
  )

(* TODO: Factorize me *)
(** [delocalize_aux dim init_zero acc_in_place acc any_mark labels index]: TODO  *)
let delocalize_aux (dim : trm) (init_zero : bool) (acc_in_place : bool) (acc : string option) (any_mark : mark) (labels : label list) (index : string) (ops : local_ops) (t : trm) : trm =
  let index = new_var index in
  match t.desc with
  | Trm_seq (tl, None) ->
    if Mlist.length tl < 5 then trm_fail t "Matrix_core.delocalize_aux: the targeted sequence does not have the correct shape";
    let add_labels = List.length labels = 3 in
    let decl = Mlist.nth tl 0 in
    begin match decl.desc with
    | Trm_let ((local_var, ty), init) ->
      begin match trm_ref_inv_init init with
      | Some alloc_trm ->
        begin match alloc_inv alloc_trm with
        | Some (_, dims, _) ->
            let alloc_arity = List.length dims in
            let new_alloc_trm = Matrix_core.insert_alloc_dim_aux dim alloc_trm in
            let new_decl = trm_let_mut (local_var, (get_inner_ptr_type ty)) new_alloc_trm in
            let snd_instr = Mlist.nth tl 1 in
            begin match trm_fors_inv alloc_arity snd_instr with
            | Some (loop_range, body) ->
              let new_dims = dim :: dims in
              let loop_range = List.map fst loop_range in
              let indices = List.fold_left (fun acc range -> (trm_var range.index) :: acc) [] (List.rev loop_range) in
              let new_indices = (trm_var index) :: indices in
              let new_loop_range = loop_range @ [{ index; start = trm_int 0; direction = DirUp; stop = dim; step = trm_step_one () }] in
              let tg = [nbAny; cCellAccess ~base:[cVarId local_var] ()] in
              let set_instr =
              begin match body.desc with
              | Trm_seq (tl, None) when Mlist.length tl = 1->
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
                      trm_for { index; start = trm_int 1; direction = DirUp; stop = dim; step = trm_step_one () } (set base new_dims new_indices init_val;)]
                    in

                  (* FIXME: typ_int on next line seems weird and is probably wrong... *)
                  let op_fun (l_arg : trm) (r_arg : trm) = trm_compound_assign ~typ:typ_int op l_arg r_arg in
                  let acc_trm =
                  if acc_in_place
                    then
                    if acc_provided
                      then trm_fail t "Matrix_core.delocalize_aux: if acc_in_place is set to true there is not need to provide an accumulator"
                      else begin
                        trm_seq_nomarks [
                          trm_set (get_operation_arg old_var_access) (trm_get (access (base) new_dims ((trm_int 0) :: indices)));
                          trm_for { index; start = trm_int 1; direction = DirUp; stop = dim; step = trm_step_one () } (op_fun (get_operation_arg old_var_access) (trm_get new_access))]
                      end
                    else
                      if not acc_provided then trm_fail t "Matrix_core.delocalize_aux: accumulator should be provided otherwise you need to set the flag ~acc_in_place to false" else
                        let acc_var = new_var acc in
                        (trm_seq_nomarks [
                          trm_let_mut (acc_var, typ_f64) init_val;
                          trm_for { index; start = trm_int 0; direction = DirUp; stop = dim; step = trm_step_one () } (trm_seq_nomarks [
                              op_fun (trm_var acc_var) (trm_get new_access)]);
                          trm_set (get_operation_arg old_var_access) (trm_var_get acc_var)]) in
                let new_fst_instr =
                  if add_labels then begin
                    let label_to_add = List.nth labels 0 in
                      if label_to_add = ""
                      then new_decl
                      else trm_add_label label_to_add (trm_seq_nobrace_nomarks [
                        trm_let_mut_uninit (local_var, (get_inner_ptr_type ty));
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
                    Path.apply_on_path (Matrix_core.insert_access_dim_index_aux dim (trm_add_mark any_mark (trm_apps (trm_var (name_to_var "ANY")) [dim]))) acc p
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
                        trm_let_mut_uninit (local_var, (get_inner_ptr_type ty));
                        (trm_set (trm_var local_var) ((trm_cast (get_inner_ptr_type ty) new_alloc_trm)))])
                    end
                  else new_decl in

                let ps1 = Constr.resolve_target tg body in
                let new_snd_instr =
                  let updated_mindex =
                  List.fold_left (fun acc p ->
                    Path.apply_on_path (Matrix_core.insert_access_dim_index_aux dim (trm_var index)) acc p
                  ) body ps1 in
                  (* TODO: Implement the case when init_zero = false *)
                  trm_fors new_loop_range updated_mindex in

                let thrd_instr = Mlist.nth tl 2 in
                let ps2 = Constr.resolve_target tg thrd_instr in
                let new_thrd_instr =
                  List.fold_left (fun acc p ->
                    Path.apply_on_path (Matrix_core.insert_access_dim_index_aux dim (trm_add_mark any_mark (trm_apps (trm_var (name_to_var "ANY")) [dim]))) acc p
                  ) thrd_instr ps2 in

                let frth_instr = Mlist.nth tl 3 in
                let new_frth_instr = begin match trm_fors_inv alloc_arity frth_instr with
                  | Some (loop_range, body) ->
                    let loop_range = List.map fst loop_range in
                    let new_loop_range = loop_range @ [{ index; start = trm_int 0; direction = DirUp; stop = dim; step = trm_step_one () }] in
                    let ps2 = Constr.resolve_target tg body in
                    let new_body =
                        List.fold_left (fun acc p ->
                      Path.apply_on_path (Matrix_core.insert_access_dim_index_aux dim (trm_var index)) acc p
                    ) body ps2  in
                    trm_fors new_loop_range new_body
                  | _ -> trm_fail t "Matrix_core.delocalize_aux: expected the accumulation loop"
                  end in

                let fifth_instr = Mlist.nth tl 4 in
                let new_fifth_instr = begin match trm_fors_inv alloc_arity fifth_instr with
                  | Some (loop_range, body) ->
                    let loop_range = List.map fst loop_range in
                    let new_loop_range = loop_range @ [{ index; start = trm_int 0; direction = DirUp; stop = dim; step = trm_step_one () }] in
                    let ps2 = Constr.resolve_target tg body in
                    let new_body =
                        List.fold_left (fun acc p ->
                      Path.apply_on_path (Matrix_core.insert_access_dim_index_aux dim (trm_var index)) acc p
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
      | _ -> trm_fail init "Matrix_core.couldn't get the alloc trms for the target local variable declaration"
      end
    | _ -> trm_fail t "Matrix_core.delocalize_aux: expected the declaration of the local variable"
    end
  |  _ -> trm_fail t "Matrix_core.delocalize_aux: expected sequence which contains the mandatory instructions for applying the delocalize transformation"

(** [delocalize ~init_zero ~acc_in_place ~acc ~dim ~index ~ops] a generalized version of variable_delocalize. *)
let%transfo delocalize ?(init_zero : bool = false) ?(acc_in_place : bool = false) ?(acc : string option) ?(any_mark : mark = no_mark) ?(labels : label list = []) ~(dim: trm) ~(index: string) ~ops:(dl_o : local_ops) (tg : target) : unit =
    Target.apply_at_target_paths (delocalize_aux dim init_zero acc_in_place acc any_mark labels index dl_o) tg

let assert_same_dims (a : trms) (b : trms) : unit =
  if not (List.for_all2 Trm_unify.are_same_trm a b) then begin
    Tools.warn "Matrix_basic: potential dimensions mismatch";
    (* DEBUG: Show.trms ~msg:"a" a;
    Show.trms ~msg:"b" b; *)
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
  let trimmed_long_dims = List.drop delta_dims long_dims in
  (* DEBUG
  Tools.debug "delta: %i" delta_dims;
  Debug.trms "trimmed long" trimmed_long_dims;
  Debug.trms "short" short_dims;
  *)
  assert_same_dims trimmed_long_dims short_dims;
  let rec compute_idxs (delta : int) (long : trms) (short : trms) : trms =
    if delta > 0 then
      (List.hd long) :: (compute_idxs (delta - 1) (List.tl long) short)
    else match (long, short) with
    | (l :: l_rest, s :: s_rest) ->
      (trm_add_int l s) :: (compute_idxs 0 l_rest s_rest)
    | ([], []) -> []
    | _ -> assert false
  in
  mindex long_dims (compute_idxs delta_dims long_idxs short_idxs)

(** [simpl_index_add]: simplifies an MINDEX(..) + MINDEX(..) expression,
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
  let (base1, i1) = match trm_array_access_inv t with
  | Some res -> res
  (* FIXME: don't want to deal with this here? *)
  | None -> trm_inv ~error trm_array_get_inv t
  in
  let (base0, i0) = trm_inv ~error trm_array_access_inv base1 in
  trm_array_access base0 (trm_add_int i0 i1)

(** [simpl_access_of_access]: simplifies &((&p[i0])[i1]) into &p[i0 + i1]

   TODO: should this be in another file?
   *)
let%transfo simpl_access_of_access ?(indepth:bool =false )(tg : target) : unit =
  Trace.justif_always_correct ();
  Trace.tag_simpl_access ();

  Target.apply_at_target_paths (maybe_trm_bottom_up_try indepth simpl_access_of_access_on) tg

(* internal *)
let find_occurences_and_add_mindex0 (x : var) (t : trm) : (bool * trm) =
  let found = ref false in
  let rec loop (t : trm) : trm =
    match trm_var_inv t with
    | Some y when x = y ->
      found := true;
      trm_array_access (trm_var ?typ:t.typ y) (mindex [] [])
    | _ -> trm_map loop t
  in
  let res_t = loop t in
  (!found, res_t)

let intro_malloc0_on (mark_alloc : mark) (mark_free : mark) (x : var) (t : trm) : trm = begin
  let instrs, result = trm_inv
    ~error:"Matrix_basic.intro_malloc0_on: expected sequence"
    trm_seq_inv t
  in
  let decl_info_opt = ref None in
  Mlist.iteri (fun i instr ->
    match trm_let_inv instr with
    | Some (y, ty, init) when x = y ->
      if Option.is_some (trm_ref_uninit_inv init)
      then begin
        assert (Option.is_none !decl_info_opt);
        decl_info_opt := Some (i, ty);
      end
    | _ -> ()
  ) instrs;
  match !decl_info_opt with
  | Some (decl_index, decl_ty) ->
    let new_decl = trm_add_mark mark_alloc (
      Matrix_core.let_alloc x (get_inner_ptr_type decl_ty) []) in
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
    let free_instr = trm_add_mark mark_free (Matrix_trm.free (trm_var x)) in
    let instrs4 = Mlist.insert_at (!last_use + 1) free_instr instrs3 in
    trm_seq ~annot:t.annot ?result instrs4
  | None -> trm_fail t "Matrix_basic.intro_malloc0_on: expected unintialized stack allocation"
end

(** [intro_malloc0]: given a target to a sequence with a declaration allocating
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
    let (common_indices, new_indices) = List.split_at ((List.length indices) - copy_dims) indices in
    begin match !common_indices_opt with
    | Some ci -> assert (List.for_all2 Trm_unify.are_same_trm ci common_indices);
    | None -> common_indices_opt := Some common_indices
    end;
    let new_dims = List.take_last copy_dims dims in
    Matrix_trm.access (trm_var stack_var) new_dims new_indices
  ) t in
  let (dims, typ) = Option.get !ret_dims_and_typ in
  let ptr_typ = typ_ptr typ in
  let common_indices = Option.get !common_indices_opt in
  let new_dims = List.take_last copy_dims dims in

  let res_pattern_before, res_pattern_after =
    if !Flags.resource_typing_enabled then
      let find_matrix_res_pattern res_list =
        let open Resource_formula in
        let var_access_fn = new_var "access" in
        let rec try_get_matrix_res_pattern formula =
          Pattern.pattern_match formula [
            Pattern.(formula_repr !__ !__) (fun ptr repr () ->
              match access_inv ptr with
              | Some ({ desc = Trm_var v }, res_dims, idxs) when var_eq v var && List.for_all2 Trm_unify.are_same_trm dims res_dims ->
                Some (formula_repr (trm_apps (trm_var var_access_fn) idxs) repr)
              | _ -> None
            );
            Pattern.(formula_group !__ !__ !__) (fun idx range body () ->
              Option.map (fun b -> formula_group idx range b) (try_get_matrix_res_pattern body)
            );
            Pattern.(formula_read_only __ !__) (fun body () -> try_get_matrix_res_pattern body);
            Pattern.__ (fun () -> None)
          ]
        in
        let error = "could not find a resource corresponding to the copied matrix" in
        var_access_fn, Option.unsome ~error (List.find_map (fun (_, formula) -> try_get_matrix_res_pattern formula) res_list.linear)
      in
      find_matrix_res_pattern (Resources.before_trm t), find_matrix_res_pattern (Resources.after_trm t)
    else
      (dummy_var, trm_unit ()), (dummy_var, trm_unit ())
  in

  (* let array_typ = List.fold_left (fun acc i -> typ_array acc (Trm i)) typ new_dims in *)
  trm_seq_nobrace_nomarks [
    (* TODO: define Matrix_core.stack_alloc, FIXME: new with dims has to be uninit? use different prim? *)
    trm_let (stack_var, typ_ptr typ) (trm_ref_uninit (typ_matrix typ new_dims));
    Matrix_core.matrix_copy_at ~typ ~matrix_res_pattern:res_pattern_before
      (trm_var ~typ:ptr_typ stack_var) [] new_dims
      (trm_var ~typ:ptr_typ var) common_indices dims;
    new_t;
    Matrix_core.matrix_copy_at ~typ ~matrix_res_pattern:res_pattern_after
      (trm_var ~typ:ptr_typ var) common_indices dims
      (trm_var ~typ:ptr_typ stack_var) [] new_dims;
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
        let t_res_before = Resource_set.filter_touched t_res_usage (Resources.before_trm t) in
        let t_res_after = Resource_set.filter_touched t_res_usage (Resources.after_trm t) in
        let used_vars = Var_set.union (Resource_set.used_vars t_res_before) (Resource_set.used_vars t_res_after) in
        if Var_set.mem var used_vars then
          trm_fail t "resources still mention replaced variable after transformation"
        else
          Trace.justif "resources do not mention replaced variable after transformation"
      end
    )) tg)

let memset_apply_on ~(depth: int) ?(typ:typ option) (t : trm) :trm =
  let for_error = "Matrix_basic.memset_apply: expected for loop"  in
  let (ranges_and_contracts, body) = trm_inv ~error:for_error (trm_fors_inv depth) t in
  let ranges = List.map fst ranges_and_contracts in
  let error = "Matrix_basic.memset_apply: expected exactly one instr in loop body" in
  let instr  = trm_inv ~error trm_seq_single_inv body in
  let error = "Matrix_basic.memset_apply: expected a set operation" in
  let (lhs,rhs) = trm_inv ~error trm_set_inv instr in
  let error = "Matrix_basic.memset_apply: expected an array affectation" in
  let (array,dims,indices) = trm_inv ~error Matrix_trm.access_inv lhs in
  let array_typ =
    match typ with
    | Some ty -> ty
    | None ->
      match array.typ with
      | Some ty -> get_inner_array_type ty
      | None -> trm_fail t "Cannot find array type for memset"
    in
  if not (List.length ranges = List.length dims) then trm_fail t "Matrix_basic.memset_apply: expect nestedness to match array dimensions";
  let check (range,(dim,indice)) : unit =
    let { index; start; direction; stop; step } = range in
    if not (direction = DirUp) then  trm_fail t  "Matrix_basic.memset_apply: expect up direction";
    if not (trm_is_zero start && trm_is_one step) then trm_fail t "Matrix_basic.memset_apply: expect start =0 and step = 1";
    if not (Trm_unify.are_same_trm stop dim) then  trm_fail t "Matrix_basic.memset_apply: expect stop to match matrix dimension";
    in
  List.iter check (List.combine ranges (List.combine dims indices));
  Matrix_core.matrix_set ~typ:array_typ rhs array dims

  (** [memset] : Uses memset instead of for-loops initialization  *)
let%transfo memset ?(depth :int option) ?(typ:typ option) (tg:target) : unit =
  apply_at_target_paths (fun t ->
    let depth =
      match depth with
      | Some nest -> nest
      | _ -> trm_fors_depth t
      in
    memset_apply_on ~depth ?typ t) tg
let elim_mindex_on_opt (simpl : trm -> trm) (t : trm) : trm option =
  let rec generate_index (acc : trm) (dims : trms) (idxs : trms) : trm =
    match (dims, idxs) with
    | (d :: dr, i :: ir) ->
      let new_acc = trm_add_int acc (List.fold_left trm_mul_int i dr) in
      generate_index new_acc dr ir
    | _ -> acc
  in
  match mindex_inv t with
  | Some (dims, idxs) -> Some (simpl (generate_index (trm_int 0) dims idxs))
  | None -> None

let elim_msize_on_opt (simpl: trm -> trm) (t : trm) : trm option =
  match msize_inv t with
  | Some dims -> Some (simpl (List.fold_right trm_mul_int dims (trm_int 1)))
  | None -> None

let elim_mops_on_opt (simpl: trm -> trm) (t : trm) =
  Option.or_else (elim_mindex_on_opt simpl t) (fun () -> elim_msize_on_opt simpl t)

let elim_mindex_on (t : trm) : trm =
  match elim_mindex_on_opt (fun t -> t) t with
  | Some t2 -> t2
  | None -> trm_fail t "expected MINDEX expression"

(** [elim_mindex] expects target [tg] to point at a call to MINDEX,
  and replaces it with the flattened index computation.

   Equivalent to:
   Rewrite.equiv_at ~ctx:true "int d1, d2, i1, i2; ==> MINDEX2(d1, d2, i1, i2) == (i1 * d2 + i2)" tg
   Rewrite.equiv_at ~ctx:true "int d1, d2, d3, i1, i2, i3; ==> MINDEX3(d1, d2, d3, i1, i2, i3) == (i1 * d2 * d3 + i2 * d3 + i3)" tg
   [...]
   *)
let%transfo elim_mindex (tg : target) : unit =
  Resources.justif_correct "size and index expressions are pure";
  Target.apply_at_target_paths elim_mindex_on tg

(** recursive version of [elim_mindex] that also removes other matrix operations like allocs, frees, and memcopies. *)
let%transfo elim_all_mops ?(simpl : trm -> trm = Arith_basic.(Arith_core.simplify false (compose [gather_rec; compute]))) (tg : target) : unit =
  Resources.justif_correct "size and index expressions are pure";
  Target.iter (fun p ->
    Target.apply_at_path (trm_bottom_up (fun t ->
      match elim_mops_on_opt simpl t with
      | Some t2 -> t2
      | None -> t
    )) p
  ) tg

let storage_folding_on (var : var) (dim : int) (n : trm) (t : trm) : trm =
  let new_dims = ref [] in
  let rec update_accesses_and_alloc (t : trm) : trm =
    match Matrix_trm.access_inv t with
    | Some (f, dims, indices) ->
      begin match trm_var_inv f with
      | Some v when v = var -> begin
        new_dims := List.update_nth dim (fun _ -> n) dims;
        let new_indices = List.update_nth dim (fun i -> trm_trunc_mod_int i n) indices in
        Matrix_trm.access ~annot:t.annot f !new_dims new_indices
        end
      | _ -> trm_map update_accesses_and_alloc t
      end
    | None ->
      begin match Matrix_core.let_alloc_inv t with
      | Some (v, etyp, dims, zero_init) when v = var ->
        let new_dims = List.update_nth dim (fun _ -> n) dims in
        trm_let ~annot:t.annot (v, typ_ptr etyp) (Matrix_trm.alloc ~zero_init etyp new_dims)
      | _ ->
        begin match trm_var_inv t with
        | Some n when n = var ->
          trm_fail t "Matrix_basic.storage_folding_on: variable access is not covered"
        | _ ->
          begin match Matrix_trm.free_inv t with
          | Some _freed -> t
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

(** [storage_folding] expects target [tg] to point at a sequence defining matrix
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
    | Some (v, vtyp, init) when var_eq v var ->
      assert (Option.is_some (Matrix_trm.alloc_inv init));
      trm_seq_nobrace_nomarks []
    | _ ->
      begin match trm_var_inv t with
      | Some n when var_eq n var ->
        trm_fail t "Matrix_basic.delete_on: matrix should not be used anymore"
      | _ ->
        let is_free_var = begin match Matrix_trm.free_inv t with
        | Some freed ->
          begin match trm_var_inv freed with
          | Some n -> var_eq n var
          | None -> false
          end
        | None -> false
        end in
        if is_free_var then trm_seq_nobrace_nomarks []
        else trm_map update_accesses_and_alloc t
      end
  in
  update_accesses_and_alloc t

(** [delete] expects target [tg] to point to a sequence defining matrix [var], and deletes it.
  Both allocation and de-allocation instructions are deleted.
  [var] should not be used anywhere, this is checked through var ids.
  TODO: additionnal check/invariant: this assumes that alloc/free dimensions are read-only expressions
   *)
let%transfo delete ~(var : var) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () ->
    Target.apply_at_target_paths (delete_on var) tg);
  Scope.infer_var_ids (); (* FIXME: redundant with trm -> trm function checks *)
  Trace.justif "matrix is not used anywhere"

(** [read_last_write]: expects the target [tg] to pint at a matrix read operation, and replaces it with the value that was last written to this matrix index. The [write] target must correspond to this last write.
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
    if not (Trm_unify.are_same_trm wr_base rd_base) then
      trm_fail t "Matrix_basic.read_last_write: array base mistmach";
    let rd_value = List.fold_left (fun value (wr_i, rd_i) ->
      begin match trm_var_inv wr_i with
      | Some wr_i_var ->
        trm_subst_var wr_i_var rd_i value
      | None ->
        let error = "Matrix_basic.read_last_write: expected write index to be a variable, or to be the same as the read index" in
        if (Trm_unify.are_same_trm wr_i rd_i) then value
        else trm_fail wr_i error
      end
    ) wr_value (List.combine wr_indices rd_indices)
    in
    rd_value
  ) tg

(** [tiles_accesses block_size nb_blocks x index_dim t]: changes all the
    occurences of the array to the tiled form.
    - [block_size] size of the blocks used for tiling
    - [nb_blocks] number of blocks used for tilling
    - [x] - var representing the array for which we want to modify the accesses
    - [index_dim] index of the tiled array's dimension
    - [t] - ast node located in the same level or deeper as the array
      declaration Assumptions:
    - x is not used in function definitions, but only in var declarations
    - the array_size is block_size * nb_blocks *)

let tile_accesses (block_size : trm) (nb_blocks : trm) (x : typvar)
    (index_dim : int) (t : trm) : trm =
  access_map
    (fun (base, dims, indices) ->
      let dimfront, current_dim, dimback =
        List.get_item_and_its_relatives index_dim dims
      in
      let ifront, current_index, iback =
        List.get_item_and_its_relatives index_dim indices
      in
      ( base,
        dimfront @ [ nb_blocks; block_size ] @ dimback,
        ifront
        @ [
            trm_trunc_div_int current_index block_size;
            trm_trunc_mod_int current_index block_size;
          ]
        @ iback ))
    x t

(** [tile_at block_name block_size index t]: transform an array declaration from
    a normal shape into a tiled one, then call apply_tiling to change all the
    array occurrences into the correct form.
    - [nb_blocks] - optional, use to indicate the nb of tiled blocks, if not
      provided, a const int is declared to compute division.
    - [block_size] the size of the tile,
    - [index] the index of the instruction inside the sequence
    - [t] - ast of the outer sequence containing the array declaration.

    Ex: t[i] -> t[i/B][i%B] *)
let tile_at ?(nb_blocks : trm option) ~(block_size : trm) ~(index_dim : int)
    (index : int) (t : trm) : trm =
  let tl, result = trm_inv trm_seq_inv t in
  let lfront, d, lback = Mlist.get_item_and_its_relatives index tl in
  let array_var, typ, typ_alloc, trms, init =
    trm_inv ~error:"Matrix_basic.tile_at : Expected an array declaration (maybe you forgot const?)"
      let_alloc_inv d
  in
  if not (0 <= index_dim && index_dim <= List.length trms) then
    trm_fail t
      "Matrix_basic.tile_at: index_dim must be betweeen the number of \
       dimensions"
  else
    let dimfront, current_dim, dimback =
      List.get_item_and_its_relatives index_dim trms
    in
    let lfront, nb_blocks =
      match nb_blocks with
      | Some x -> (lfront, x)
      | None ->
          let new_block_var = name_to_var (array_var.name ^ "_blocks") in
          (* nb_blocks = ceil(array_size / block_size)
             where ceil(a/b) is computed as (a+b -1) /b *)
          let new_block =
            trm_let (new_block_var, typ_int)
              (trm_trunc_div_int
                 (trm_add_int current_dim (trm_sub_int block_size (trm_int 1)))
                 block_size)
          in
          (Mlist.push_back new_block lfront, trm_var new_block_var)
    in
    let new_dims = dimfront @ [ nb_blocks; block_size ] @ dimback in
    let new_d =
      trm_let (array_var, typ)
        (Matrix_trm.alloc ~zero_init:init typ_alloc new_dims)
    in
    let lback =
      Mlist.map
        (fun t -> (tile_accesses block_size nb_blocks array_var index_dim) t)
        lback
    in
    let new_items = Mlist.merge (Mlist.push_back new_d lfront) lback in
    trm_seq ?result new_items
