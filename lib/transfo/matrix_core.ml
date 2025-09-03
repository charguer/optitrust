open Ast
open Trm
open Typ
open Contextualized_error
open Mark
open Matrix_trm

(** list of (offset, size) *)
type nd_tile = (trm * trm) list

(** list of (start, end) *)
type nd_range = (trm * trm) list

(* TODO?
(** [tile_none]: special tile value where the dimension should be kept fully *)
let tile_all: trm * trm = trm_int 1, trm_int 0

(** [tile_none]: special tile value where the dimension should be dropped *)
let tile_none: trm * trm = trm_int 0, trm_int 0
*)

let let_alloc ?(annot : trm_annot = trm_annot_default) ?(zero_init = false) (v : var) (ty : typ) (dims : trms) : trm =
  trm_let (v, typ_ptr ty) (alloc ~zero_init ty dims)

let let_alloc_inv (t : trm) : (var * typ * trms * bool) option =
  Option.bind (trm_let_inv t) (fun (v, _, init) ->
  Option.bind (alloc_inv init) (fun (elem_t, dims, zeroinit) ->
    Some (v, elem_t, dims, zeroinit)
  ))

let let_alloc_uninit_inv (t : trm) : (var * typ * trms) option =
  Option.bind (trm_let_inv t) (fun (v, _, init) ->
  Option.bind (alloc_uninit_inv init) (fun (elem_t, dims) ->
    Some (v, elem_t, dims)
  ))

let matrix_copy_var nb_dims typ : var =
  toplevel_var (sprintf "MATRIX%d_COPY_%s" nb_dims (Ast_to_c.typ_to_string typ))

let matrix_copy ~(typ: typ) (dest: trm) (src: trm) (dims: trm list) : trm =
  let nb_dims = List.length dims in
  let copy_var = matrix_copy_var nb_dims typ in
  trm_apps (trm_var copy_var) (dest :: src :: dims)


let matrix_set_var nb_dims typ =
  toplevel_var (sprintf "MATRIX%d_MEMSET_%s" nb_dims (Ast_to_c.typ_to_string typ))
let matrix_set ~(typ: typ) (value:trm) (dest: trm) (dims: trm list) : trm=
  let nb_dims = List.length dims in
  let set_var = matrix_set_var nb_dims typ in
  trm_apps (trm_var set_var) (dest::value::dims)



(** [matrix_copy_at dest d_offset d_dims src s_offset s_dims copy_dims] uses a series
    of ghosts to issue a [MATRIX*_COPY_*] from matrix [dest] at [d_offset] to matrix [src] at [s_offset].
  let matrix_set ..
    Assumes both dest and src resources are already focussed on the region to be copied.
    This is often the case because of surrounding loops, else you can independantly add ghosts to focus the relevant rows.

    Preconditions:
    - [dest] has dims [d_dims] and [(len d_offset) <= (len d_dims)]
    - [src] has dims [s_dims] and [(len s_offset) <= (len s_dims)]
    - [(len d_dims) - (len d_offset) = (len s_dims) - (len s_offset) = (len copy_dims)]
    - [take_last (len copy_dims) d_dims = take_last (len copy_dims) s_dims]
  *)
let matrix_copy_at ~(typ: typ) ~(matrix_res_pattern: var * formula)
  (dest : trm) (d_indices : trm list) (d_dims : trm list)
   (src : trm) (s_indices : trm list) (s_dims : trm list) : trm =
  let remove_res_pattern_dim (access_var: var) (res_pattern: formula) =
    let lower_access_var = new_var "access" in
    let rec remove_access_dim t =
      Pattern.pattern_match t [
        Pattern.(trm_apps (trm_specific_var access_var) (__ ^:: !__) __ __) (fun next_idxs () -> trm_apps (trm_var lower_access_var) next_idxs);
        Pattern.__ (fun () -> trm_map remove_access_dim t)
      ]
    in
    lower_access_var, remove_access_dim res_pattern
  in
  let res_pattern_as_fun (dims: trm list) (access_var: var) (res_pattern: formula) =
    Resource_formula.(formula_fun [access_var, typ_pure_simple_fun (List.map (fun _ -> typ_int) dims) (typ_ptr typ)] res_pattern)
  in

  let dest, d_dims, ghosts_before, ghosts_after, _ = List.fold_left (fun (dest, d_dims, ghosts_before, ghosts_after, (access_var, res_pattern)) index ->
      match d_dims with
      | [] -> failwith "matrix_copy: more offset coordinates than dimensions for dest"
      | top_dim :: next_dims ->
        let new_dest = trm_array_access dest (List.fold_left trm_mul_int index next_dims) in
        let new_ghost_before = Resource_trm.ghost (ghost_mindex_unfold dest d_dims (res_pattern_as_fun d_dims access_var (Resource_formula.formula_uninit res_pattern))) in
        let new_ghost_after = Resource_trm.ghost (ghost_mindex_fold dest d_dims (res_pattern_as_fun d_dims access_var res_pattern)) in
        (new_dest, next_dims, new_ghost_before :: ghosts_before, new_ghost_after :: ghosts_after, remove_res_pattern_dim access_var res_pattern)
    ) (dest, d_dims, [], [], matrix_res_pattern) d_indices in

  let src, s_dims, ghosts_before, ghosts_after, _ = List.fold_left (fun (src, s_dims, ghosts_before, ghosts_after, (access_var, res_pattern)) index ->
      match s_dims with
      | [] -> failwith "matrix_copy: more offset coordinates than dimensions for src"
      | top_dim :: next_dims ->
        let new_src = trm_array_access src (List.fold_left trm_mul_int index next_dims) in
        let _, new_ghost_before, new_ghost_after = Resource_trm.ghost_pair (ghost_ro_mindex_unfold src s_dims (res_pattern_as_fun s_dims access_var res_pattern)) in
        (new_src, next_dims, new_ghost_before :: ghosts_before, new_ghost_after :: ghosts_after, remove_res_pattern_dim access_var res_pattern)
    ) (src, s_dims, ghosts_before, ghosts_after, matrix_res_pattern) s_indices in

    if not (List.equal Trm_unify.are_same_trm d_dims s_dims) then failwith "matrix_copy: different sizes for dest and src after accesses: %s != %s" (Tools.list_to_string (List.map Ast_to_c.ast_to_string d_dims)) (Tools.list_to_string (List.map Ast_to_c.ast_to_string s_dims));
    Nobrace.trm_seq_nomarks ((List.rev ghosts_before) @ matrix_copy ~typ dest src d_dims :: ghosts_after)

(** [map_all_accesses v dims map_indices mark t] maps all accesses to [v] in [t],
    using the [map_access] function.

    Fails if [v] occurs in a sub-term that is not an access to [v], as this could mean some
    accesses are hidden (e.g. behind a function call).

    - the dimensions and type of the matrix [v] are stored in [ret_dims_and_typ] if provided.
    *)
let map_all_accesses (v : var) ?(ret_dims_and_typ : (trms * typ) option ref option)
  (map_access : trms -> trms -> trm) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match access_inv t with
    | Some (f, dims, indices) ->
      begin match trm_var_inv f with
      | Some x when var_eq x v ->
        Option.iter (fun rdt ->
          if Option.is_none !rdt then begin
            let typ = Option.get (typ_ptr_inv (Option.get f.typ)) in
            rdt := Some (dims, typ);
          end;
        ) ret_dims_and_typ;
        map_access dims indices
      | _ -> trm_map aux t
      end
    | None ->
      begin match trm_var_inv t with
      | Some x when x = v ->
        trm_fail t "map_all_accesses: variable access is not covered"
      | _ -> trm_map aux t
      end
  in
  aux t

(** [replace_all_accesses prev_v v dims map_indices mark t] replaces all accesses to [prev_v]
    in [t] with accesses to [v], using new [dims] and changing indices with [map_indices].
    *)
let replace_all_accesses (prev_v : var) (v : var) (dims : trm list)
  ?(ret_dims_and_typ : (trms * typ) option ref option)
  (map_indices : (trm -> trm) list) (mark : mark) (t : trm) : trm =
  map_all_accesses prev_v ?ret_dims_and_typ (fun prev_dims prev_indices ->
    let indices = List.map2 (fun m i -> m i) map_indices prev_indices in
    let typ = Option.bind ret_dims_and_typ (fun dt ->
      (* best effort type recovery .. *)
      Option.map (fun (d, t) -> t) !dt
    ) in
    trm_add_mark mark (access (trm_var ?typ v) dims indices)
  ) t

(** [pointwise_fors ?reads ?writes ?modifies ranges body] creates nested loops
  with [ranges] over the main body [body].
  The body has the given [reads], [writes], and [preserves].
  Each loop contract adds a layer of pointwise Group resources.
  *)
let pointwise_fors
  ?(reads: formula list = [])
  ?(writes: formula list = [])
  ?(preserves: formula list = [])
  (ranges : loop_range list) (body : trm) : trm =
  let (t, _, _, _) = List.fold_right (fun range (t, reads, writes, preserves) ->
    let push_clauses clause formulas contract =
      List.fold_left (fun contract formula ->
        let res = (Resource_formula.new_anon_hyp (), formula) in
        Resource_contract.push_loop_contract_clause clause res contract
      ) contract formulas
    in
    let contract = empty_strict_loop_contract
      |> push_clauses (Exclusive Reads) reads
      |> push_clauses (Exclusive Writes) writes
      |> push_clauses (Exclusive Preserves) preserves
    in
    let t' = trm_for ~contract range (if (is_trm_seq t) then t else trm_seq_nomarks [t]) in
    let reads' = List.map (Resource_formula.formula_group_range range) reads in
    let writes' = List.map (Resource_formula.formula_group_range range) writes in
    let preserves' = List.map (Resource_formula.formula_group_range range) preserves in
    (t', reads', writes', preserves')
  ) ranges (body, reads, writes, preserves)
  in
  t


(****************************************************************************************************)
(*                        Core transformations on C matrices                                        *)
(****************************************************************************************************)



(** [reorder_dims_aux order t]: reorders the dimensions in a call to MSIZE or MINDEX,
      [order] - a list of indices based on which the elements in dims should be ordered,
      [t] - ast of the call to MALLOC or MINDEX. *)
let reorder_dims_aux (rotate_n : int) (order : int list) (t : trm) : trm =
  let typ_alloc = ref (trm_int 1) in
  let init_alloc = ref false in
  let dims, indices =
    match mindex_inv t with
    | Some (dims, indices) -> dims, Some indices
    | None -> match alloc_inv t with
      | Some (typ,dims, init) -> typ_alloc := typ; init_alloc := init; dims,None
      | None -> trm_fail t "Matrix_core.reorder_dims_aux: expected a function call to MSIZE or MINDEX"
    in
  let nb = List.length dims in
  let order = if rotate_n <> 0
    then let id_perm = List.range 0 (nb - 1) in
          List.rotate rotate_n id_perm
    else
      begin match order with
    | [] -> trm_fail t "Matrix_core.reorder_dims_aux: permuation order of indices and dims should be given or ~rotate_n argument should be used"
    | _ -> order
    end in
  begin try List.check_permutation nb order with | List.Invalid_permutation -> trm_fail t "Matrix_core.order is not a permutation of indices" end;
  let reordered_dims = List.reorder order dims in
  match indices with
  | Some indices ->
    let reordered_indices = List.reorder order indices in
    mindex reordered_dims reordered_indices
  | None ->
    alloc !typ_alloc reordered_dims ~zero_init:!init_alloc


(** [insert_alloc_dim_aux new_dim t]: adds a new dimension at the beginning of the list of dimension,
     [new_dim]: the new dimension which is going to be inserted into the list of dims in an allocation,
     [t]: ast of the allocation. *)
let insert_alloc_dim_aux ?(last : bool = false) (new_dim : trm) (t : trm) : trm =
  match alloc_inv t with
  | Some (ty, dims, zero_init) ->
    let new_dims = if last then dims @ [new_dim] else new_dim :: dims in
    alloc ~zero_init ty new_dims
  | None -> trm_fail t "Matrix_core.insert_alloc_dim_aux: expected a matrix allocation"

(** [insert_access_dim_index_aux new_dim new_index t]: add a new dimension at the beginning of the list of dimension
     and add a new index at the begining of the list of indices in the call to MINDEX inside the
     targeted array access.
      [new_dim]: the new dimension which is goin to be inserted into the list of dims in call to MINDEX,
      [new_index]: the new index which is goin to be inserted into the list of indices in call to MINDEX,
      [t]: ast of the array_access with the updated list of args in the call to MINDEX. *)
let insert_access_dim_index_aux ?(last : bool = false) (new_dim : trm) (new_index : trm) (t : trm) : trm =
  match access_inv t with
  | Some (base, dims, indices) ->
    let new_dims = if last then dims @ [new_dim] else new_dim :: dims in
    let new_indices = if last then indices @ [new_index] else new_index :: indices in
    access base new_dims new_indices
  | None -> trm_fail t "Matrix_core.insert_access_dim_index_aux: expected an array access "

(** [local_name_aux mark var local_var malloc_trms var_type t] insert a local matrix declaration with name [local_var] and copy the content
      from the matrix [var] to [local_var] and replace all the accesses of that matrix inside the targeted insturction [t].
      [mark] - an optional mark at the final generated sequence,
      [var] - the name of the current matrix used in instruction [t],
      [new_var] - the name of the local matrix which replaces all the current accesses of [var],
      [t] - ast of thee instuction which contains accesses to [var]. *)
(* TODO: superseded by tile version *)
let local_name_aux (mark : mark)
  (var : var) (local_var : string)
  (dims : trms) (elem_type : typ) (indices : string list)
  (local_ops : local_ops) (t : trm) : trm =
  let local_var = new_var local_var in
  let fst_instr = let_alloc local_var elem_type dims in
  let indices_list = begin match indices with
  | [] -> List.mapi (fun i _ -> "i" ^ (string_of_int (i + 1))) dims | _ as l -> l  end in
  let indices_list = List.map new_var indices_list in
  let indices = List.map (fun ind -> trm_var ind) indices_list in
  let nested_loop_range = List.map2 (fun dim ind-> { index = ind; start = (trm_int 0); direction = DirUp; stop = dim; step = trm_step_one () }) dims indices_list in
  begin match local_ops with
    | Local_arith _ ->
      let write_on_local_var =
        trm_set (access (trm_var local_var) dims indices) (trm_get (access (trm_var var) dims indices)) in
      let write_on_var =
        trm_set (access (trm_var var) dims indices) (trm_get (access (trm_var local_var) dims indices)) in
      let snd_instr = trm_copy (trm_fors nested_loop_range write_on_local_var) in
      let new_t = trm_subst_var var (trm_var local_var) t in
      let thrd_instr = trm_copy (trm_fors nested_loop_range write_on_var) in
      let last_instr = free (trm_var local_var) in
      let final_trm = Nobrace.trm_seq_nomarks [fst_instr; snd_instr; new_t; thrd_instr; last_instr] in
      trm_add_mark mark final_trm
    | Local_obj (init, swap, free_fn) ->
      let write_on_local_var =
        trm_apps (trm_var init)  [access (trm_var local_var) dims indices] in
      let write_on_var =
        trm_apps (trm_var swap) [access (trm_var var) dims indices; access (trm_var local_var) dims indices] in
      let free_local_var =
        trm_apps (trm_var free_fn)  [access (trm_var local_var) dims indices] in
      let snd_instr = trm_copy (trm_fors nested_loop_range write_on_local_var) in
      let new_t = trm_subst_var var (trm_var local_var) t in
      let thrd_instr = trm_copy (trm_fors nested_loop_range write_on_var) in
      let frth_instr = trm_copy (trm_fors nested_loop_range free_local_var) in
      let last_instr = free (trm_var local_var) in
      let final_trm = Nobrace.trm_seq_nomarks [fst_instr; snd_instr; new_t; thrd_instr; frth_instr; last_instr] in
      trm_add_mark mark final_trm
    end
