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
  let mindex1 = trm_inv ~error Matrix_core.mindex_inv a in
  let mindex2 = trm_inv ~error Matrix_core.mindex_inv b in
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
      (trm_add l s) :: (compute_idxs 0 l_rest s_rest)
    | ([], []) -> []
    | _ -> assert false 
  in
  Matrix_core.mindex long_dims (compute_idxs delta_dims long_idxs short_idxs)

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
  let (base1, i1) = trm_inv ~error array_access_inv t in
  let (base0, i0) = trm_inv ~error array_access_inv base1 in
  array_access base0 (trm_add i0 i1)

(* [simpl_access_of_access]: simplifies &((&p[i0])[i1]) into &p[i0 + i1]

   TODO: should this be in another file?
   *)
let simpl_access_of_access : Target.Transfo.t =
  Target.apply_at_target_paths simpl_access_of_access_on