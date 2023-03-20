open Ast

(* [intro_calloc tg]: expects the target [tg] to point at  a call to funciton alloc then it will
    replace this call with a call to CALLOC. *)
let intro_calloc : Target.Transfo.t =
  Target.apply_on_targets (Matrix_core.intro_calloc)

(* [intro_malloc tg]: expects the target [tg] to point at a call to the function MALLOC,
      then it will replace this call with a call to MALLOC. *)
let intro_malloc : Target.Transfo.t =
  Target.apply_on_targets (Matrix_core.intro_malloc)

(* [intro_mindex dim tg]. expects the target [tg] to point at an array access
    then it will replace that access to let say index i with an access at
    MINDEX (dim,i). *)
let intro_mindex (dim : trm) : Target.Transfo.t =
  Target.apply_on_targets (Matrix_core.intro_mindex dim)

(* [reorder_dims order tg]: expects the target [tg] to point at a call to ALLOC or MINDEX functions,
      then it will reorder their args based on [order], where [order] is a list of indices which the
      current args should follow. *)
let reorder_dims ?(rotate_n : int = 0) ?(order : int list = [])  (): Target.Transfo.t =
  Target.apply_on_targets (Matrix_core.reorder_dims rotate_n order)

(* [insert_alloc_dim new_dim]: expects the target [tg] to point at call to ALLOC functions, then it will
      add a new arg at the begining of the list of args in the targeted call. *)
let insert_alloc_dim (new_dim : trm) : Target.Transfo.t =
  Target.apply_on_targets (Matrix_core.insert_alloc_dim new_dim)

(* [insert_access_dim new_dim new_index tg]: expects the target [tg] to point at an array access, then it will
    add two new args([new_dim] and [new_index]) in the call to MINDEX function inside that array access. *)

let insert_access_dim_index (new_dim : trm) (new_index : trm) : Target.Transfo.t =
  Target.apply_on_targets (Matrix_core.insert_access_dim_index new_dim new_index)

(* [biject fun_name tg]: expectes the target [tg] to point at a function call, then it replaces the name
     of the called function with [fun_name]. *)
let biject (fun_name : string) : Target.Transfo.t =
  Expr.replace_fun fun_name

(* [local_name ~mark var into tg]: expects the target to point at an instruction that contains
      an occurrence of [var] then it will define a matrix [into] whose dimensions will be the same
      as the one of [var]. Then we copy the contents of the matrix [var] into [into] and finally we
      free up the memory. *)
let local_name ?(my_mark : mark option) ?(indices : (var list) = []) ?(alloc_instr : Target.target option = None) (v : var) ~into:(into : var) ?(local_ops : local_ops = Local_arith (Lit_int 0, Binop_add)) (tg : Target.target) : unit =
  let remove = (my_mark = None) in
  let get_alloc_type_and_trms (t : trm) (tg1 : Target.target) : typ * (trms * trm * bool) =
    let var_type = begin match t.desc with
      | Trm_let (_, (_, ty), _) -> get_inner_ptr_type ty
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
      let seq_tg = Target.target_of_path seq_p in
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

(* [delocalize ~init_zero ~acc_in_place ~acc ~dim ~index ~ops] a generalized version of variable_delocalize. *)
let delocalize ?(init_zero : bool = false) ?(acc_in_place : bool = false) ?(acc : string option) ?(any_mark : mark = "") ?(labels : label list = []) ~dim:(dim : trm)  ~index:(index : string) ~ops:(dl_o : local_ops) : Target.Transfo.t =
    Target.apply_on_targets (Matrix_core.delocalize dim init_zero acc_in_place acc any_mark labels index dl_o)

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
  Printf.printf "trimmed long: %s\n" (Tools.list_to_string
    (List.map AstC_to_c.ast_to_string trimmed_long_dims));
  Printf.printf "\n       short: %s\n" (Tools.list_to_string
  (List.map AstC_to_c.ast_to_string short_dims));
  *)
  (* TODO: need something better for term equality *)
  let dims_matching = List.combine trimmed_long_dims short_dims |>
    List.for_all (fun (a, b) -> a.desc = b.desc)
  in
  if not dims_matching then
    fail None "Matrix_basic.simpl_index_on: dimensions mismatch";
  let rec compute_idxs (delta : int) (long : trms) (short : trms) : trms =
    if delta > 0 then
      (List.hd long) :: (compute_idxs (delta - 1) (List.tl long) short)
    else match (long, short) with
    | (l :: l_rest, s :: s_rest) -> 
      (trm_add ~typ:l.typ l s) :: (compute_idxs 0 l_rest s_rest)
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
let simpl_index_add : Target.Transfo.t =
  Target.apply_at_target_paths simpl_index_add_on

let simpl_access_of_access_on (t : trm) : trm =
  let error = "Matrix_basic.simpl_access_of_access_on: expected nested array accesses" in
  (* Printf.printf "debug:\n%s\n" (AstC_to_c.ast_to_string t); *)
  let (base1, i1) = match array_access_inv t with
  | Some res -> res
  (* FIXME: don't want to deal with this here? *)
  | None -> trm_inv ~error array_get_inv t
  in
  let (base0, i0) = trm_inv ~error array_access_inv base1 in
  array_access base0 (trm_add ~typ:i0.typ i0 i1)

(* [simpl_access_of_access]: simplifies &((&p[i0])[i1]) into &p[i0 + i1]

   TODO: should this be in another file?
   *)
let simpl_access_of_access : Target.Transfo.t =
  Target.apply_at_target_paths simpl_access_of_access_on

(* internal *)
let find_occurences_and_add_mindex0 (x : var) (t : trm) : (bool * trm) =
  let found = ref false in
  let rec loop (t : trm) : trm =
    match trm_var_inv t with 
    | Some (_, y) when x = y ->
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
    let instrs4 = Mlist.insert_at (!last_use + 1) (trm_free (trm_var_get x)) instrs3 in
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
     ... uses x ...
     free(x);
     ...
   }

   LATER: deal with control flow
  
   *)
let intro_malloc0 (x : var) : Target.Transfo.t =
  Target.apply_at_target_paths (intro_malloc0_on x)

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
let stack_copy_on (name : string) (stack_name : string) (d : int) (t : trm) : trm =
  let dims_and_typ_opt : (trms * typ) option ref = ref None in
  let common_indices_opt : trms option ref = ref None in
  let rec update_accesses (t : trm) : trm =
    match Matrix_core.access_inv t with
    | Some (f, dims, indices) ->
      begin match trm_var_get_inv f with
      | Some (_, n) when n = name -> begin
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
      | Some (_, n) when n = name ->
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

let stack_copy ~(name : string) ~(stack_name : string) ~(d : int) (tg : Target.target) : unit =
  Internal.nobrace_remove_after (fun () ->
    Target.apply_at_target_paths (stack_copy_on name stack_name d) tg)