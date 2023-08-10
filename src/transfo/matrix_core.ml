open Syntax
open Target

(* list of (offset, size) *)
type nd_tile = (trm * trm) list

(* TODO?
(* [tile_none]: special tile value where the dimension should be kept fully *)
let tile_all: trm * trm = trm_int 1, trm_int 0

(* [tile_none]: special tile value where the dimension should be dropped *)
let tile_none: trm * trm = trm_int 0, trm_int 0
*)

(* [access t dims indices]: builds the a matrix access with the index defined by macro [MINDEX], see [mindex] function.
    Ex: x[MINDEX(N1,N2,N3, i1, i2, i3)]. *)
let access ?(annot : trm_annot = trm_annot_default) (t : trm) (dims : trms) (indices : trms) : trm =
  let mindex_trm = mindex dims indices in
  trm_apps ~annot (trm_binop Binop_array_access) [t; mindex_trm]

(* [access_inv t]: returns the array access base, the list of dimensions and indices used as args at matrix access [t]. *)
let access_inv (t : trm) : (trm * trms * trms) option=
  match t.desc with
  | Trm_apps (f, [base;index]) ->
    begin match trm_prim_inv f with
    | Some (Prim_binop Binop_array_access) ->
      begin match mindex_inv index with
      | Some (dm, ind) -> Some (base, dm, ind)
      | _ -> None
      end
    | _ -> None
    end
  | _ -> None

(* [get base dims indices]: takes the trm built from access function and puts it into a get operation. *)
let get (base : trm) (dims : trms) (indices : trms) : trm =
  let access_trm = access base dims indices in
  trm_apps (trm_unop Unop_get) [access_trm]

(* [get_inv t]: gets the trm inside a get oepration on an access. *)
let get_inv (t : trm) : (trm * trms * trms) option =
  match t.desc with
  | Trm_apps (_f,[base]) when is_get_operation t -> access_inv base
  | _ -> None

(* [set base dims indices arg]: creates a set operation on which the address where the write is done
    is an access trm built with function accesses and [arg] is the value which is written to that
    that address. *)
let set (base : trm) (dims : trms) (indices : trms) (arg : trm) : trm =
  let write_trm = access base dims indices in
  trm_apps (trm_binop (Binop_set)) [write_trm; arg]

(* [set_inv t]: returns the arguments used in the function [set]. *)
let set_inv (t : trm) : (trm * trms * trms * trm)  option =
  match t.desc with
  | Trm_apps (_f, [addr;v]) when is_set_operation t ->
    begin match access_inv addr with
    | Some (base, dims, indices) -> Some (base, dims, indices, v)
    | None -> None
    end
  | _ -> None

(* |alloc ~init dims size]: creates a call to function the MALLOC$(N) and CALLOC$(N) where [N] is the
     number of dimensions and [size] is the size in bytes occupied by a single matrix element in
     the memeory. *)
let alloc ?(init : trm option) (dims : trms) (size : trm) : trm =
  let n = List.length dims in
  match init with
  | Some _ -> trm_apps (trm_toplevel_var ("CALLOC" ^  (string_of_int n))) (dims @ [size])
  | None -> trm_apps (trm_toplevel_var ("MALLOC" ^  (string_of_int n))) (dims @ [size])


let alloc_with_ty ?(annot : trm_annot = trm_annot_default) (dims : trms) (ty : typ) : trm =
  let n = List.length dims in
  let size = trm_toplevel_var ("sizeof(" ^ (AstC_to_c.typ_to_string ty) ^ ")") in
  trm_cast ~annot (typ_ptr Ptr_kind_mut ty) (
    trm_apps (trm_toplevel_var ("MALLOC" ^  (string_of_int n))) (dims @ [size]))

let alloc_inv_with_ty (t : trm) : (trms * typ * trm)  option =
  Option.bind (trm_new_inv t) (fun (_, t2) ->
  Option.bind (trm_cast_inv t2) (fun (ty, t3) ->
  Option.bind (trm_apps_inv t3) (fun (f, args) ->
  Option.bind (trm_var_inv f) (fun f_var ->
    if Tools.pattern_matches "MALLOC" f_var.name
    then begin
      let dims, size = Xlist.unlast args in
      Some (dims, Option.get (typ_ptr_inv ty), size)
    end else None
  ))))

(* |alloc_aligned ~init dims size alignment] create a call to function MALLOC_ALIGNED$(N) where [N] is the
     number of dimensions and [size] is the size in bytes occupied by a single matrix element in
     the memory and [alignment] is the alignment size. *)
let alloc_aligned (dims : trms) (size : trm) (alignment : trm)  : trm =
  let n = List.length dims in
  trm_apps (trm_toplevel_var ("MALLOC_ALIGNED" ^  (string_of_int n))) (dims @ [size; alignment])


(* [zero_initialized]: a boolean type used as flag to tell if the array cells should be initialized to zero or not. *)
type zero_initialized = bool


(* [alloc_inv t]:  returns all the args used in function alloc [t]. *)
let alloc_inv (t : trm) : (trms * trm * zero_initialized)  option=
  match t.desc with
  | Trm_apps (f, args) ->
    begin match f.desc with
    | Trm_var (_, f_var) ->
      let dims , size = Xlist.unlast args in
      if (Tools.pattern_matches "CALLOC" f_var.name) then Some (dims, size, true)
        else if (Tools.pattern_matches "MALLOC" f_var.name) then Some (dims, size, false)
        else None
    | _ -> None
    end
  | _ -> None

(* [vardef_alloc_inv t ] returns all the args used in vardef_alloc*)
let vardef_alloc_inv (t : trm) : (var * typ * trms * trm * zero_initialized) option =
  match t.desc with
  | Trm_let (_, (x, ty), init, _) ->
    begin match get_init_val init with
    | Some init1 ->
      begin match alloc_inv  init1 with
      | Some (dims, size, z_in) -> Some (x, (get_inner_ptr_type ty), dims, size, z_in)
      | _ -> None
      end
    | _ -> None
    end

  | _ -> None

let free (dims : trms) (t : trm) : trm =
  let n = List.length dims in
  trm_apps (trm_toplevel_var ("MFREE" ^  (string_of_int n))) (dims @ [t])

let free_inv (t : trm) : trm option =
  Option.bind (trm_apps_inv t) (fun (f, args) ->
  Option.bind (trm_var_inv f) (fun f_var ->
    if Tools.pattern_matches "MFREE" f_var.name
    then begin
      let _dims, t = Xlist.unlast args in
      Some t
    end else None
  ))

(* [replace_all_accesses]: replace all accesses to [prev_v] in [t] with accesses to [v], using new [dims] and changing indices with [map_indices].*)
let replace_all_accesses (prev_v : var) (v : var) (dims : trm list) (map_indices : (trm -> trm) list) (mark : mark option) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match access_inv t with
    | Some (f, prev_dims, prev_indices) ->
      begin match trm_var_get_inv f with
      | Some n when n = prev_v ->
        let indices = List.map2 (fun m i -> m i) map_indices prev_indices in
        trm_may_add_mark mark (access (trm_var_get v) dims indices)
      | _ -> trm_map aux t
      end
    | None ->
      begin match trm_var_inv t with
      | Some n when n = prev_v ->
        fail t.loc "Matrix_core.replace_all_accesses: variable access is not covered"
      | _ -> trm_map aux t
      end
  in
  aux t

(****************************************************************************************************)
(*                        Core transformations on C matrices                                        *)
(****************************************************************************************************)

(* [intro_calloc_aux t]: replaces a call to calloc with a call to the macro CALLOC,
     [t] - ast of the call to alloc. *)
let intro_calloc_aux (t : trm) : trm =
  match t.desc with
  | Trm_apps ({desc = Trm_var (_, f);_},[dim; size]) when f.name = "calloc" ->
    alloc ~init:(trm_int 0) [dim] size
  | _ -> fail t.loc "Matrix_core.intro_calloc_aux: expected a function call to calloc"

let intro_calloc : Target.Transfo.local =
  Target.apply_on_path (intro_calloc_aux)


(* [intro_malloc_aux t]: replaces a call to calloc with a call to MALLOC,
     [t] - ast of the call to alloc. *)
let intro_malloc_aux (t : trm) : trm =
  match t.desc with
  | Trm_apps ({desc = Trm_var (_, f);_},[{desc = Trm_apps (_,[dim ;size]);_}]) when (var_has_name f "malloc") ->
    alloc [dim] size
  | _ -> fail t.loc "Matrix_core.intro_malloc: expected a function call to malloc"


(* [intro_malloc t p]: applies [intro_malloc_aux] at trm [t] with path [p]. *)
let intro_malloc : Target.Transfo.local =
  Target.apply_on_path (intro_malloc_aux)


(* [intro_mindex_aux dim t] replaces an array access at index [i] with an array access at MINDEX([dim],i),
      [dim] - the size of the array accesses with [t],
      [t] - the ast of the array access. *)
let intro_mindex_aux (dim : trm) (t : trm) : trm =
  match t.desc with
  | Trm_apps (f, [base;index]) ->
    begin match trm_prim_inv f with
    | Some (Prim_binop Binop_array_access) ->
      trm_apps ~annot:t.annot f [base; mindex [dim] [index]]
    | _ -> fail t.loc "Matrix_core.intro_mindex_aux: expected a primitive array access operation"
    end
  | _ -> fail t.loc "Matrix_core.intro_mindex_aux: expected an array access trm got %s"

(* [intro_mindex dim t p]: applies [intro_mindex_aux] at trm [t] with path [p]. *)
let intro_mindex (dim : trm) : Target.Transfo.local =
  Target.apply_on_path (intro_mindex_aux dim)

(* [reorder_dims_aux order t]: reorders the dimensions in a call to CALLOC, MALLOC or MINDEX,
      [order] - a list of indices based on which the elements in dims should be ordered,
      [t] - ast of the call to CALLOC, MALLOC, MINDEX. *)
let reorder_dims_aux (rotate_n : int) (order : int list) (t : trm) : trm =
  match mindex_inv t, alloc_inv t with
  | Some (dims, indices), None ->
    let nb = List.length dims in
    let order = if rotate_n <> 0
      then let id_perm = Xlist.range 0 (nb - 1) in
           Xlist.rotate rotate_n id_perm
      else
        begin match order with
      | [] -> fail t.loc "Matrix_core.reorder_dims_aux: permuation order of indices and dims should be given or ~rotate_n argument should be used"
      | _ -> order
      end in
    begin try Xlist.check_permutation nb order with | Xlist.Invalid_permutation -> fail t.loc "Matrix_core.order is not a permutation of indices" end;
    let reordered_dims = Xlist.reorder order dims in
    let reordered_indices = Xlist.reorder order indices in
    mindex (reordered_dims) (reordered_indices)
  | None, Some (dims, size, zero_init) ->
    let nb = List.length dims in
    let order = if rotate_n <> 0
      then let id_perm = Xlist.range 0 (nb - 1) in
           Xlist.rotate rotate_n id_perm
      else
        begin match order with
      | [] -> fail t.loc "Matrix_core.reorder_dims_aux: permuation order of indices and dims should be given or ~rotate_n argument should be used"
      | _ -> order
      end in
    begin try Xlist.check_permutation nb order with | Xlist.Invalid_permutation -> fail t.loc "Matrix_core.order is not a permutation of indices" end;
    let reordered_dims = Xlist.reorder order dims in
    let init = if zero_init then Some (trm_int 0 ) else None in
    alloc ?init reordered_dims size
  | _ -> fail t.loc "Matrix_core.reorder_dims_aux: expected  a function call to CALLOC or MINDEX"

(* [reorder_dims rotate_n order t p]: applies [reorder_dims_aux] at trm [t] with path [p]. *)
let reorder_dims (rotate_n : int ) (order : int list) : Target.Transfo.local =
  Target.apply_on_path (reorder_dims_aux rotate_n order)


(* [insert_alloc_dim_aux new_dim t]: adds a new dimension at the beginning of the list of dimension,
     [new_dim]: the new dimension which is goin to be inserted into the list of dims in call to CALLOC or MALLOC,
     [t]: ast of the call to ALLOC functions. *)
let insert_alloc_dim_aux ?(last : bool = false) (new_dim : trm) (t : trm) : trm =
  match alloc_inv t with
  | Some (dims, size, zero_init) ->
    let new_dims = if last then dims @ [new_dim] else new_dim :: dims in
    let init = if zero_init then Some (trm_int 0) else None in
    alloc ?init new_dims size
  | None -> fail t.loc "Matrix_core.insert_alloc_dim_aux: expected a function call to CALLOC"

let insert_alloc_dim (new_dim : trm) : Target.Transfo.local =
  Target.apply_on_path (insert_alloc_dim_aux new_dim)

(* [insert_access_dim_index_aux new_dim new_index t]: add a new dimension at the beginning of the list of dimension
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
  | None -> fail t.loc "Matrix_core.insert_access_dim_index_aux: expected an array access "

(* [insert_access_dim_inidex new_timd new_index t p]: applies [insert_access_dim_index_aux] at trm [t] with path [p]. *)
let insert_access_dim_index (new_dim : trm) (new_index : trm) : Target.Transfo.local =
  Target.apply_on_path (insert_access_dim_index_aux new_dim new_index)


(* [local_name_aux mark var local_var malloc_trms var_type t] insert a local matrix declaration with name [local_var] and copy the content
      from the matrix [var] to [local_var] and replace all the accesses of that matrix inside the targeted insturction [t].
      [mark] - an optional mark at the final generated sequence,
      [var] - the name of the current matrix used in instruction [t],
      [new_var] - the name of the local matrix which replaces all the current accesses of [var],
      [t] - ast of thee instuction which contains accesses to [var]. *)

(* TODO: superseded by tile version *)
let local_name_aux (mark : mark option) (var : var) (local_var : var) (malloc_trms : trms * trm * bool) (var_type : typ) (indices : (var list) )(local_ops : local_ops) (t : trm) : trm =
  let dims, size, zero_init = malloc_trms in
  let local_var_type = var_type in
  let init = if zero_init then Some (trm_int 0) else None in
  let fst_instr = trm_let_mut (local_var,local_var_type) (trm_cast (local_var_type) (alloc ?init dims size )) in
  let indices_list = begin match indices with
  | [] -> List.mapi (fun i _ -> new_var ("i" ^ (string_of_int (i + 1)))) dims | _ as l -> l  end in
  let indices = List.map (fun ind -> trm_var ind) indices_list in
  let nested_loop_range = List.map2 (fun dim ind-> (ind, (trm_int 0), DirUp,  dim, Post_inc, false)) dims indices_list in
  begin match local_ops with
    | Local_arith _ ->
      let write_on_local_var =
        trm_set (access (trm_var_get local_var) dims indices) (trm_get (access (trm_var_get var) dims indices)) in
      let write_on_var =
        trm_set (access (trm_var_get var) dims indices) (trm_get (access (trm_var_get local_var) dims indices)) in
      let snd_instr = trm_fors nested_loop_range write_on_local_var in
      let new_t = Subst.subst_var var (trm_var local_var) t in
      let thrd_instr = trm_fors nested_loop_range write_on_var in
      let last_instr = free dims (trm_var_get local_var) in
      let final_trm = trm_seq_no_brace [fst_instr; snd_instr; new_t; thrd_instr; last_instr] in
      begin match mark with Some m -> trm_add_mark m final_trm | _ ->  final_trm end
    | Local_obj (init, swap, free_fn) ->
      let write_on_local_var =
        trm_apps (trm_var init)  [access (trm_var_get local_var) dims indices] in
      let write_on_var =
        trm_apps (trm_var swap) [access (trm_var_get var) dims indices; access (trm_var_get local_var) dims indices] in
      let free_local_var =
        trm_apps (trm_var free_fn)  [access (trm_var_get local_var) dims indices] in
      let snd_instr = trm_fors nested_loop_range write_on_local_var in
      let new_t = Subst.subst_var var (trm_var local_var) t in
      let thrd_instr = trm_fors nested_loop_range write_on_var in
      let frth_instr = trm_fors nested_loop_range free_local_var in
      let last_instr = free dims (trm_var_get local_var) in
      let final_trm = trm_seq_no_brace [fst_instr; snd_instr; new_t; thrd_instr; frth_instr; last_instr] in
      begin match mark with Some m -> trm_add_mark m final_trm | _ ->  final_trm end
    end


(* TODO: superseded by tile version, except when accesses are not visible (new_t = subst_var does not work) *)
(* [local_name mark var local_var malloc_trms var_type indices local_ops t p]: applies [local_name_aux] at trm [t] with path [p]. *)
let local_name (mark : mark option) (var : var) (local_var : var) (malloc_trms :trms * trm * bool) (var_type : typ) (indices : var list ) (local_ops : local_ops) : Target.Transfo.local =
  Target.apply_on_path (local_name_aux mark var local_var malloc_trms var_type indices local_ops)

(* TODO: Factorize *)
let local_name_tile_aux (mark : mark option) (mark_accesses : mark option) (var : var) (tile : nd_tile) (local_var : var) (malloc_trms : trms * trm * bool) (var_type : typ) (indices : (var list) )(local_ops : local_ops) (t : trm) : trm =
  let dims, size, zero_init = malloc_trms in
  let local_var_type = var_type in
  let init = if zero_init then Some (trm_int 0) else None in
  let indices_list = begin match indices with
  | [] -> List.mapi (fun i _ -> "i" ^ (string_of_int (i + 1))) dims | _ as l -> l  end in
  let indices = List.map (fun ind -> trm_var ind) indices_list in
  let nested_loop_range = List.map2 (fun (offset, size) ind -> (ind, offset, DirUp, trm_add offset size, Post_inc, false)) tile indices_list in
  let tile_dims = List.map (fun (_, size) -> size) tile in
  let tile_indices = List.map2 (fun (offset, _) ind -> trm_sub ind offset) tile indices in
  let alloc_instr = trm_let_mut (local_var,local_var_type) (trm_cast (local_var_type) (alloc ?init tile_dims size )) in
  let map_indices = List.map (fun (offset, size) -> fun i -> trm_sub i offset) tile in
  let new_t = replace_all_accesses var local_var tile_dims map_indices mark_accesses t in
  let final_trm = begin match local_ops with
    | Local_arith _ ->
      let write_on_local_var =
        trm_set (access (trm_var_get local_var) tile_dims tile_indices) (trm_get (access (trm_var_get var) dims indices)) in
      let write_on_var =
        trm_set (access (trm_var_get var) dims indices) (trm_get (access (trm_var_get local_var) tile_dims tile_indices)) in
      let load_for = trm_fors nested_loop_range write_on_local_var in
      let unload_for = trm_fors nested_loop_range write_on_var in
      let free_instr = free tile_dims (trm_var_get local_var) in
      trm_seq_no_brace [alloc_instr; load_for; new_t; unload_for; free_instr]
    | Local_obj (init, swap, free_fn) ->
      let write_on_local_var =
        trm_apps (trm_var init)  [access (trm_var_get local_var) tile_dims tile_indices] in
      let write_on_var =
        trm_apps (trm_var swap) [access (trm_var_get var) dims indices; access (trm_var_get local_var) tile_dims tile_indices] in
      let free_local_var =
        trm_apps (trm_var free_fn)  [access (trm_var_get local_var) tile_dims tile_indices] in
      let snd_instr = trm_fors nested_loop_range write_on_local_var in
      let thrd_instr = trm_fors nested_loop_range write_on_var in
      let frth_instr = trm_fors nested_loop_range free_local_var in
      let last_instr = free tile_dims (trm_var_get local_var) in
      trm_seq_no_brace [alloc_instr; snd_instr; new_t; thrd_instr; frth_instr; last_instr]
  end in
  trm_may_add_mark mark final_trm


(* [local_name_tile]: applies [local_name_tile_aux] at trm [t] with path [p]. *)
let local_name_tile (mark : mark option) (mark_accesses : mark option) (var : var) (tile : nd_tile) (local_var : var) (malloc_trms :trms * trm * bool) (var_type : typ) (indices : var list ) (local_ops : local_ops) : Target.Transfo.local =
  Target.apply_on_path (local_name_tile_aux mark mark_accesses var tile local_var malloc_trms var_type indices local_ops)


(* TODO: Factorize me *)

(* [delocalize_aux dim init_zero acc_in_place acc any_mark labels index]: TODO  *)
let delocalize_aux (dim : trm) (init_zero : bool) (acc_in_place : bool) (acc : string option) (any_mark : mark) (labels : label list) (index : string) (ops : local_ops) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    if Mlist.length tl < 5 then fail t.loc "Matrix_core.delocalize_aux: the targeted  sequence does not have the correct shape";
    let add_labels = List.length labels = 3 in
    let decl = Mlist.nth tl 0 in
    begin match decl.desc with
    | Trm_let (_, (local_var, ty), init, _) ->
      begin match get_init_val init with
      | Some init1 ->
        begin match init1.desc with
         | Trm_apps (_, [alloc_trm]) ->
          begin match alloc_inv alloc_trm with
          | Some (dims, _, _) ->
              let alloc_arity = List.length dims in
              let new_alloc_trm = insert_alloc_dim_aux dim alloc_trm in
              let new_decl = trm_let_mut (local_var, (get_inner_ptr_type ty)) (trm_cast (get_inner_ptr_type ty) new_alloc_trm) in
              let snd_instr = Mlist.nth tl 1 in
              begin match trm_fors_inv alloc_arity snd_instr with
              | Some (loop_range, body) ->
                let new_dims = dim :: dims in
                let indices = List.fold_left (fun acc (ind, _, _, _, _, _) -> (trm_var ind) :: acc) [] (List.rev loop_range) in
                let new_indices = (trm_var index) :: indices in
                let new_loop_range = loop_range @ [(index, trm_int 0, DirUp, dim, Post_inc, false)] in
                let tg = [nbAny; cCellAccess ~base:[cVar local_var] ()] in
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
                        trm_for (index, (trm_int 1), DirUp, dim, (Post_inc), false) (set base new_dims new_indices init_val;)]
                      in

                    let op_fun (l_arg : trm) (r_arg : trm) = trm_prim_compound op l_arg r_arg in
                    let acc_trm  =
                    if acc_in_place
                      then
                      if acc_provided
                        then fail t.loc "Matrix_core.delocalize_aux: if acc_in_place is set to true there is not need to provide an accumulator"
                        else begin
                          trm_seq_nomarks [
                           trm_set (get_operation_arg old_var_access) (trm_get (access (base) new_dims ((trm_int 0) :: indices)));
                           trm_for (index, (trm_int 1), DirUp, dim, (Post_inc), false) ( op_fun (get_operation_arg old_var_access) (trm_get new_access))]
                        end
                      else
                        if not acc_provided then fail t.loc "Matrix_core.delocalize_aux: accumulator should be provided otherwise you need to set the flag ~acc_in_place to false" else
                          (trm_seq_nomarks [
                            trm_let_mut (acc, typ_double ()) init_val;
                            trm_for (index, (trm_int 0), DirUp, dim, (Post_inc), false) (trm_seq_nomarks [
                                op_fun (trm_var acc) (trm_get new_access)]);
                            trm_set (get_operation_arg old_var_access) (trm_var_get acc)]) in
                  let new_fst_instr =
                    if add_labels then begin
                      let label_to_add = List.nth labels 0 in
                        if label_to_add = ""
                        then new_decl
                        else trm_add_label label_to_add (trm_seq_no_brace [
                          trm_let_mut (local_var, (get_inner_ptr_type ty)) (trm_uninitialized ());
                          (trm_set (trm_var local_var) ((trm_cast (get_inner_ptr_type ty) new_alloc_trm)))])
                      end
                    else new_decl in

                  let new_snd_instr = if init_zero
                    then trm_fors new_loop_range init_trm
                    else trm_fors loop_range init_trm in

                  let thrd_instr = Mlist.nth tl 2 in
                  let ps2 = resolve_target tg thrd_instr in
                  let new_thrd_instr =
                    List.fold_left (fun acc p ->
                      apply_on_path (insert_access_dim_index_aux dim (trm_add_mark any_mark (trm_apps (trm_var "ANY") [dim]))) acc p
                    ) thrd_instr ps2 in

                  let new_frth_instr =
                    trm_fors loop_range acc_trm in

                  let fifth_instr = Mlist.nth tl 4 in
                  let new_fifth_instr = if add_labels then
                     let label_to_add = List.nth labels 2 in
                     if label_to_add = "" then fifth_instr else trm_add_label label_to_add fifth_instr
                      else fifth_instr in

                    trm_seq ~annot:t.annot (Mlist.of_list [new_fst_instr; new_snd_instr; new_thrd_instr; new_frth_instr; new_fifth_instr])
                  | _ -> fail set_instr.loc "Matrix_core.delocalize_aux"
                  end
                | Local_obj (_init_f, _merge_f, free_f) ->

                  let new_fst_instr =
                    if add_labels then begin
                      let label_to_add = List.nth labels 0 in
                        if label_to_add = ""
                        then new_decl
                        else (trm_seq_no_brace [
                          trm_let_mut (local_var, (get_inner_ptr_type ty)) (trm_uninitialized ());
                          (trm_set (trm_var local_var) ((trm_cast (get_inner_ptr_type ty) new_alloc_trm)))])
                      end
                    else new_decl in

                  let ps1 = resolve_target tg body in
                  let new_snd_instr =
                    let updated_mindex =
                    List.fold_left (fun acc p ->
                      apply_on_path (insert_access_dim_index_aux dim (trm_var index)) acc p
                    ) body ps1 in
                    (* TODO: Implement the case when init_zero = false *)
                    trm_fors new_loop_range updated_mindex in

                  let thrd_instr = Mlist.nth tl 2 in
                  let ps2 = resolve_target tg thrd_instr in
                  let new_thrd_instr =
                    List.fold_left (fun acc p ->
                      apply_on_path (insert_access_dim_index_aux dim (trm_add_mark any_mark (trm_apps (trm_var "ANY") [dim]))) acc p
                    ) thrd_instr ps2 in

                  let frth_instr = Mlist.nth tl 3 in
                  let new_frth_instr = begin match trm_fors_inv alloc_arity frth_instr with
                    | Some (loop_range, body) ->
                      let new_loop_range = loop_range @ [(index, trm_int 0, DirUp, dim, Post_inc, false)] in
                      let ps2 = resolve_target tg body in
                      let new_body =
                          List.fold_left (fun acc p ->
                        apply_on_path (insert_access_dim_index_aux dim (trm_var index)) acc p
                      ) body ps2  in
                      trm_fors new_loop_range new_body
                    | _ -> fail t.loc "Matrix_core.delocalize_aux: expected the accumulation loop"
                    end in

                  let fifth_instr = Mlist.nth tl 4 in
                  let new_fifth_instr = begin match trm_fors_inv alloc_arity fifth_instr with
                    | Some (loop_range, body) ->
                      let new_loop_range = loop_range @ [(index, trm_int 0, DirUp, dim, Post_inc, false)] in
                      let ps2 = resolve_target tg body in
                      let new_body =
                          List.fold_left (fun acc p ->
                        apply_on_path (insert_access_dim_index_aux dim (trm_var index)) acc p
                      ) body ps2  in
                      trm_fors new_loop_range new_body
                    | _ -> fail t.loc "Matrix_core.delocalize_aux: expected the accumulation loop"
                    end in

                  let sixth_instr = Mlist.nth tl 5 in
                    let final_groups =
                      if List.length labels = 0 then [new_fst_instr; new_snd_instr; new_thrd_instr; new_frth_instr; new_fifth_instr; sixth_instr]
                       else List.mapi ( fun i lb ->
                        let new_subsgroup = if i = 0
                          then trm_seq_no_brace [new_fst_instr; new_snd_instr]
                          else if i = 1 then trm_seq_no_brace [new_thrd_instr; new_frth_instr]
                          else trm_seq_no_brace [new_fifth_instr; sixth_instr]
                          in
                        if lb = "" then new_subsgroup else trm_add_label lb new_subsgroup

                       ) labels
                    in
                  trm_seq ~annot:t.annot (Mlist.of_list final_groups)
                end

              | _ -> fail snd_instr.loc "Matrix_core.delocalize_aux: expected the nested loops where the local matrix initialization is done"
              end
          | _ -> fail init.loc "Matrix_core.delocalize_aux: the local variable should be declared together with its mermory allocation"
          end
         | _ -> fail init1.loc "Matrix_core.delocalize_aux: couldn't find the cast operation "
        end

      | _ -> fail init.loc "Matrix_core.couldn't get the alloc trms for the target local variable declaration"
      end
    | _ -> fail t.loc "Matrix_core.delocalize_aux: expected the declaration of the local variable"
    end
  |  _ -> fail t.loc "Matrix_core.delocalize_aux: expected sequence which contains the mandatory instructions for applying the delocalize transformation"


(* [delocalize dim init_zero acc_in_place acc any_mark labels index ops t p]: applies [delocalize_aux] at trm [t] with path [p]. *)
let delocalize (dim : trm) (init_zero : bool) (acc_in_place : bool) (acc : string option) (any_mark : mark) (labels : label list) (index : string) (ops : local_ops): Target.Transfo.local =
  Target.apply_on_path (delocalize_aux dim init_zero acc_in_place acc any_mark labels index ops)
