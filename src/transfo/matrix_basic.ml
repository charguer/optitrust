open Ast
open Target

(* [intro_calloc tg]: expects the target [tg] to point at  a call to funciton alloc then it will
    replace this call with a call to CALLOC. *)
let%transfo intro_calloc (tg : target) : unit =
  Target.apply_on_targets (Matrix_core.intro_calloc) tg

(* [intro_malloc tg]: expects the target [tg] to point at a call to the function MALLOC,
      then it will replace this call with a call to MALLOC. *)
let%transfo intro_malloc (tg : target) : unit =
  Target.apply_on_targets (Matrix_core.intro_malloc) tg

(* [intro_mindex dim tg]. expects the target [tg] to point at an array access
    then it will replace that access to let say index i with an access at
    MINDEX (dim,i). *)
let%transfo intro_mindex (dim : trm) (tg : target) : unit =
  Target.apply_on_targets (Matrix_core.intro_mindex dim) tg

(* [reorder_dims order tg]: expects the target [tg] to point at a call to ALLOC or MINDEX functions,
      then it will reorder their args based on [order], where [order] is a list of indices which the
      current args should follow. *)
let%transfo reorder_dims ?(rotate_n : int = 0) ?(order : int list = []) (tg : target) : unit =
  Target.apply_on_targets (Matrix_core.reorder_dims rotate_n order) tg

(* [insert_alloc_dim new_dim]: expects the target [tg] to point at call to ALLOC functions, then it will
      add a new arg at the begining of the list of args in the targeted call. *)
let%transfo insert_alloc_dim (new_dim : trm) (tg : target) : unit =
  Target.apply_on_targets (Matrix_core.insert_alloc_dim new_dim) tg

(* [insert_access_dim new_dim new_index tg]: expects the target [tg] to point at an array access, then it will
    add two new args([new_dim] and [new_index]) in the call to MINDEX function inside that array access. *)

let%transfo insert_access_dim_index (new_dim : trm) (new_index : trm) (tg : target) : unit =
  Target.apply_on_targets (Matrix_core.insert_access_dim_index new_dim new_index) tg

(* [biject fun_name tg]: expectes the target [tg] to point at a function call, then it replaces the name
     of the called function with [fun_name]. *)
let%transfo biject (fun_name : string) (tg : target) : unit =
  Expr.replace_fun fun_name tg

(* TODO: implement using local_name_tile to avoid duplication *)
(* [local_name ~mark var into tg]: expects the target to point at an instruction that contains
      an occurrence of [var] then it will define a matrix [into] whose dimensions will be the same
      as the one of [var]. Then we copy the contents of the matrix [var] into [into] and finally we
      free up the memory. *)
let%transfo local_name ?(my_mark : mark option) ?(indices : (var list) = []) ?(alloc_instr : target option) (v : var) ~into:(into : var) ?(local_ops : local_ops = Local_arith (Lit_int 0, Binop_add)) (tg : target) : unit =
  let remove = (my_mark = None) in
  let get_alloc_type_and_trms (t : trm) (tg1 : target) : typ * (trms * trm * bool) =
    let var_type = begin match t.desc with
      | Trm_let (_, (_, ty), _, _) -> get_inner_ptr_type ty
      | Trm_apps (_, [lhs; _rhs]) when is_set_operation t ->
        begin match lhs.typ with
        | Some ty -> ty
        | None -> fail t.loc (Printf.sprintf "Matrix_basic.get_alloc_type_and_trms: couldn't findd the type of variable %s\n'" v)
        end
      | _ -> fail t.loc (Printf.sprintf "Matrix_basic.get_alloc_type_and_trms: couldn't findd the type of variable %s, alloc_instr
          target doesn't point to a write operation or a variable declaration \n'" v)
      end in
      let alloc_trms = begin match Target.get_trm_at (tg1 @ [Target.cFun ~regexp:true ".ALLOC."]) with
        | Some at ->
          begin match Matrix_core.alloc_inv at with
          | Some (dims, sz, zero_init) -> (dims, sz, zero_init)
          | _ -> fail t.loc "Matrix_basic.get_alloc_type_and_trms: couldn't get the dimensions and the size of the matrix"
          end
        | None -> fail None "Matrix_basic.get_alloc_type_and_trms: couldn't get the dimensions and the size of the matrix"
        end in (var_type, alloc_trms)
    in
  Internal.nobrace_remove_after ~remove (fun _ ->
    Target.(apply_on_targets (fun t p ->
      let seq_p, _ = Internal.isolate_last_dir_in_seq p in
      let seq_tg = target_of_path seq_p in
      let var_target = cOr [[cVarDef v]; [cWriteVar v]] in
      begin match alloc_instr with
      | Some tg1 ->
        begin match get_trm_at tg1 with
        | Some t1 ->
          let var_type, alloc_trms = get_alloc_type_and_trms t1 tg1 in
          if not remove then Internal.nobrace_enter();
          Matrix_core.local_name my_mark v into alloc_trms var_type indices local_ops t p
        | None -> fail None "Matrix_basic.local_name: alloc_instr target does not match to any ast node"
        end
      | None ->
        begin match get_trm_at (seq_tg @ [var_target]) with
        | Some t1 ->
          let tg1 = (seq_tg @ [var_target]) in
          let var_type, alloc_trms = get_alloc_type_and_trms t1 tg1 in
          if not remove then Internal.nobrace_enter();
          Matrix_core.local_name my_mark v into alloc_trms var_type indices local_ops t p

        | None -> fail None "Matrix_basic.local_name: alloc_instr target does not match to any ast node"
        end
      end
    ) tg)
  )

(* [local_name_tile ~mark var into tg]: expects the target to point at an instruction that contains
      an occurrence of [var] then it will define a matrix [into] whose dimensions will correspond to a tile of [var]. Then we copy the contents of the matrix [var] into [into] according to the given tile offsets and finally we free up the memory. *)
let%transfo local_name_tile ?(mark : mark option) ?(mark_accesses : mark option) ?(indices : (var list) = []) ?(alloc_instr : target option) (v : var) ~into:(into : var) (tile : Matrix_core.nd_tile) ?(local_ops : local_ops = Local_arith (Lit_int 0, Binop_add)) (tg : target) : unit =
  let remove = (mark = None) in
  let get_alloc_type_and_trms (t : trm) (tg1 : target) : typ * (trms * trm * bool) =
    let var_type = begin match t.desc with
      | Trm_let (_, (_, ty), _, _) -> get_inner_ptr_type ty
      | Trm_apps (_, [lhs; _rhs]) when is_set_operation t ->
        begin match lhs.typ with
        | Some ty -> ty
        | None -> fail t.loc (Printf.sprintf "Matrix_basic.get_alloc_type_and_trms: couldn't findd the type of variable %s\n'" v)
        end
      | _ -> fail t.loc (Printf.sprintf "Matrix_basic.get_alloc_type_and_trms: couldn't findd the type of variable %s, alloc_instr
          target doesn't point to a write operation or a variable declaration \n'" v)
      end in
      let alloc_trms = begin match Target.get_trm_at (tg1 @ [Target.cFun ~regexp:true ".ALLOC."]) with
        | Some at ->
          begin match Matrix_core.alloc_inv at with
          | Some (dims, sz, zero_init) -> (dims, sz, zero_init)
          | _ -> fail t.loc "Matrix_basic.get_alloc_type_and_trms: couldn't get the dimensions and the size of the matrix"
          end
        | None -> fail None "Matrix_basic.get_alloc_type_and_trms: couldn't get the dimensions and the size of the matrix"
        end in (var_type, alloc_trms)
    in
  Internal.nobrace_remove_after ~remove (fun _ ->
    Target.(apply_on_targets (fun t p ->
      let seq_p, _ = Internal.isolate_last_dir_in_seq p in
      let seq_tg = target_of_path seq_p in
      let var_target = cOr [[cVarDef v]; [cWriteVar v]] in
      begin match alloc_instr with
      | Some tg1 ->
        begin match get_trm_at tg1 with
        | Some t1 ->
          let var_type, alloc_trms = get_alloc_type_and_trms t1 tg1 in
          if not remove then Internal.nobrace_enter();
          Matrix_core.local_name_tile mark mark_accesses v tile into alloc_trms var_type indices local_ops t p
        | None -> fail None "Matrix_basic.local_name: alloc_instr target does not match to any ast node"
        end
      | None ->
        begin match get_trm_at (seq_tg @ [var_target]) with
        | Some t1 ->
          let tg1 = (seq_tg @ [var_target]) in
          let var_type, alloc_trms = get_alloc_type_and_trms t1 tg1 in
          if not remove then Internal.nobrace_enter();
          Matrix_core.local_name_tile mark mark_accesses v tile into alloc_trms var_type indices local_ops t p

        | None -> fail None "Matrix_basic.local_name: alloc_instr target does not match to any ast node"
        end
      end
    ) tg)
  )

(* [delocalize ~init_zero ~acc_in_place ~acc ~dim ~index ~ops] a generalized version of variable_delocalize. *)
let%transfo delocalize ?(init_zero : bool = false) ?(acc_in_place : bool = false) ?(acc : string option) ?(any_mark : mark = "") ?(labels : label list = []) ~dim:(dim : trm)  ~index:(index : string) ~ops:(dl_o : local_ops) (tg : target) : unit =
    Target.apply_on_targets (Matrix_core.delocalize dim init_zero acc_in_place acc any_mark labels index dl_o) tg

let assert_same_dims (a : trms) (b : trms) : unit =
  (* TODO: need something better for term equality *)
  if not (List.for_all2 Internal.same_trm a b) then begin
    Printf.printf "WARNING: Matrix_basic: dimensions mismatch\n";
    Debug_transfo.trms "a" a;
    Debug_transfo.trms "a" b;
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
  Trace.justif "correct when size and index expressions are pure (TODO: check)";
  Trace.tag_simpl_access ();
  Target.apply_at_target_paths simpl_index_add_on tg

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
      trm_array_access (trm_var_get y) (mindex [] [])
    | _ -> trm_map loop t
  in
  let res_t = loop t in
  (!found, res_t)

let intro_malloc0_on (x : var) (t : trm) : trm = begin
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
    let new_decl = trm_let_mut (x, decl_ty) (Matrix_core.alloc_with_ty [] (get_inner_ptr_type decl_ty)) in
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
    let instrs4 = Mlist.insert_at (!last_use + 1) (Matrix_core.free (trm_var_get x)) instrs3 in
    trm_seq ~annot:t.annot instrs4
  | None -> fail t.loc "Matrix_basic.intro_malloc0_on: expected unintialized stack allocation"
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
let%transfo intro_malloc0 (x : var) (tg : target) : unit =
  Trace.justif_always_correct ();
  Target.apply_at_target_paths (intro_malloc0_on x) tg

(*
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
(* TODO: rename name to var and stack_name to copy_name, rename d to copy_dims *)
(* TODO: Matrix.local_name_tile + Matrix.to_array *)
let stack_copy_on (name : string) (stack_name : string) (d : int) (t : trm) : trm =
  let dims_and_typ_opt : (trms * typ) option ref = ref None in
  let common_indices_opt : trms option ref = ref None in
  let rec update_accesses (t : trm) : trm =
    match Matrix_core.access_inv t with
    | Some (f, dims, indices) ->
      begin match trm_var_get_inv f with
      | Some n when n = name -> begin
        if Option.is_none !dims_and_typ_opt then begin
          let typ = Option.get (typ_ptr_inv (Option.get f.typ)) in
          dims_and_typ_opt := Some (dims, typ);
        end;
        let (common_indices, new_indices) = Xlist.split_at d indices in
        begin match !common_indices_opt with
        | Some ci -> assert (List.for_all2 Internal.same_trm ci common_indices);
        | None -> common_indices_opt := Some common_indices
        end;
        List.fold_left (fun acc i ->
          trm_array_access acc i) (trm_var_get stack_name) new_indices
        end
      | _ -> trm_map update_accesses t
      end
    | None ->
      begin match trm_var_inv t with
      | Some n when n = name ->
        fail t.loc "Matrix_basic.stack_copy_on: variable access is not covered"
      | _ -> trm_map update_accesses t
      end
  in
  let new_t = update_accesses t in
  let (dims, typ) = Option.get !dims_and_typ_opt in
  let common_indices = Option.get !common_indices_opt in
  let new_dims = Xlist.take_last d dims in
  let array_typ = List.fold_left (fun acc i ->
    typ_array acc (Trm i)
  ) typ new_dims in
  let copy_offset = trm_array_access (trm_var_get name) (mindex dims (common_indices @ (List.init d (fun _ -> trm_int 0)))) in
  let copy_size = trm_var ("sizeof(" ^ (AstC_to_c.typ_to_string array_typ) ^ ")") in
  trm_seq_no_brace [
    trm_let_mut (stack_name, array_typ) (trm_uninitialized ());
    trm_apps (trm_var "memcpy") [trm_var_get stack_name; copy_offset; copy_size];
    new_t;
    trm_apps (trm_var "memcpy") [copy_offset; trm_var_get stack_name; copy_size];
  ]

let%transfo stack_copy ~(var : string) ~(copy_var : string) ~(copy_dims : int) (tg : target) : unit =
  Internal.nobrace_remove_after (fun () ->
    Target.apply_at_target_paths (stack_copy_on var copy_var copy_dims) tg)

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
  let rec update_accesses_and_alloc (t : trm) : trm =
    match Matrix_core.access_inv t with
    | Some (f, dims, indices) ->
      begin match trm_var_get_inv f with
      | Some v when v = var -> begin
        let new_dims = Xlist.update_nth dim (fun _ -> n) dims in
        let new_indices = Xlist.update_nth dim (fun i -> trm_mod i n) indices in
        Matrix_core.access ~annot:t.annot f new_dims new_indices
        end
      | _ -> trm_map update_accesses_and_alloc t
      end
    | None ->
      begin match trm_let_inv t with
      | Some (_kind, v, vtyp, init) when v = var ->
        begin match Matrix_core.alloc_inv_with_ty init with
        | Some (dims, etyp, size) ->
          let new_dims = Xlist.update_nth dim (fun _ -> n) dims in
          trm_let_mut ~annot:t.annot (v, (get_inner_ptr_type vtyp)) (Matrix_core.alloc_with_ty new_dims etyp)
        | _ -> trm_map update_accesses_and_alloc t
        end
      | _ ->
        begin match trm_var_inv t with
        | Some n when n = var ->
          fail t.loc "Matrix_basic.storage_folding_on: variable access is not covered"
        | _ ->
          let is_free_var = begin match Matrix_core.free_inv t with
          | Some freed ->
            begin match trm_var_get_inv freed with
            | Some n -> n = var
            | None -> false
            end
          | None -> false
          end in
          if is_free_var then t
          else trm_map update_accesses_and_alloc t
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
    match trm_let_inv t with
    | Some (_kind, v, vtyp, init) when v = var ->
      (* TODO: deal with CALLOC *)
      assert (Option.is_some (Matrix_core.alloc_inv_with_ty init));
      trm_seq_no_brace []
    | _ ->
      begin match trm_var_inv t with
      | Some n when n = var ->
        fail t.loc "Matrix_basic.delete_on: matrix should not be used anymore"
      | _ ->
        let is_free_var = begin match Matrix_core.free_inv t with
        | Some freed ->
          begin match trm_var_get_inv freed with
          | Some n -> n = var
          | None -> false
          end
        | None -> false
        end in
        if is_free_var then trm_seq_no_brace []
        else trm_map update_accesses_and_alloc t
      end
  in
  update_accesses_and_alloc t

(* [delete] expects target [tg] to poitn to a sequence defining matrix [var], and deletes it.
  Both allocation and de-allocation instructions are deleted.
  Checks that [var] is not used anywhere.
   *)
let%transfo delete ~(var : var) (tg : target) : unit =
  Internal.nobrace_remove_after (fun () ->
    Target.apply_at_target_paths (delete_on var) tg)

(* [read_last_write]: expects the target [tg] to pint at a matrix read operation, and replaces it with the value that was last written to this matrix index. The [write] target must correspond to this last write.
  For correctness, if [V] was written at index [i], reading [V[j/i]] should be equivalent to reading at index [j].
   *)
let%transfo read_last_write ~(write : target) (tg : target) : unit =
  let write_trm = match Target.get_trm_at write with
  | Some wt -> wt
  | None -> fail None "Matrix_basic.read_least_write: write target not found"
  in
  let (wr_base, wr_dims, wr_indices, wr_value) = trm_inv
    ~error:"Matrix_basic.read_last_write: targeted matrix write operation is not supported"
    Matrix_core.set_inv write_trm
  in
  Target.apply_at_target_paths (fun t ->
    let (rd_base, rd_dims, rd_indices) = trm_inv
      ~error:"Matrix_basic.read_last_write: targeted matrix read operation is not supported"
      Matrix_core.get_inv t
    in
    assert_same_dims wr_dims rd_dims;
    if not (Internal.same_trm wr_base rd_base) then
      fail t.loc "Matrix_basic.read_last_write: array base mistmach";
    let rd_value = List.fold_left (fun value (wr_i, rd_i) ->
      begin match trm_var_inv wr_i with
      | Some wr_i_var ->
        Subst.subst_var wr_i_var rd_i value
      | None ->
        let error = "Matrix_basic.read_last_write: expected write index to be a variable, or to be the same as the read index" in
        if (Internal.same_trm wr_i rd_i) then value
        else fail wr_i.loc error
      end
    ) wr_value (List.combine wr_indices rd_indices)
    in
    rd_value
  ) tg
