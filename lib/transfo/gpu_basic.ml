open Prelude
open Target
open Flags

(* Launch-related variables *)

let var_kernel_launch = toplevel_var "kernel_launch"
let var_kernel_setup_end = toplevel_var "kernel_setup_end"
let var_kernel_teardown_begin = toplevel_var "kernel_teardown_begin"
let var_kernel_kill = toplevel_var "kernel_kill"
let ghost_var_kernel_teardown_sync = toplevel_var "kernel_teardown_sync"

(* Common ghosts *)

let ghost_desync_tile_divides_trivial items tile_count tile_size =
  Resource_trm.ghost (ghost_call (toplevel_var "desync_tile_divides")
    ([("items", items);
    ("div_check", (trm_apps ~annot:Resource_formula.formula_annot (trm_var (toplevel_var "eq_refl")) [trm_mul_int tile_count tile_size]));
    ("tile_count", tile_count); ("tile_size", tile_size)]))

let ghost_desync_untile_divides_trivial items tile_count tile_size =
  Resource_trm.ghost (ghost_call (toplevel_var "desync_untile_divides")
    ([("items", items);
    ("div_check", (trm_apps ~annot:Resource_formula.formula_annot (trm_var (toplevel_var "eq_refl")) [trm_mul_int tile_count tile_size]));
    ("tile_count", tile_count); ("tile_size", tile_size)]))

let ghost_dmindex_tile dims res_pattern =
  Resource_trm.ghost (ghost_call (toplevel_var (sprintf "dmindex%d_tile" (List.length dims))) (("H", res_pattern) :: List.mapi (fun i dim -> sprintf "n%d" (i+1), dim) dims))

let ghost_dmindex_untile dims res_pattern =
  Resource_trm.ghost (ghost_call (toplevel_var (sprintf "dmindex%d_untile" (List.length dims))) (("H", res_pattern) :: List.mapi (fun i dim -> sprintf "n%d" (i+1), dim) dims))

let extract_ptr_from_resource (f: formula): var option =
  let open Resource_formula in
  let rec nested_group_inv (f: formula): formula =
    Pattern.pattern_match f [
      Pattern.(formula_group __ (formula_range (trm_int (eq 0)) __ (trm_int (eq 1))) !__)
        (fun inner_formula () -> nested_group_inv inner_formula);
      Pattern.(formula_desyncgroup __ __ !__)
        (fun inner_formula () -> nested_group_inv inner_formula);
      Pattern.__ (fun () -> f)
    ] in
  let inner_formula = nested_group_inv f in
  match (formula_repr_inv inner_formula) with
  | Some ({desc = Trm_var v}, _) -> Some v
  | Some (loc, _) -> (
    match (Matrix_trm.access_inv loc) with
    | Some ({desc = Trm_var v}, _, _) -> Some v
    | _ -> None)
  | _ -> None

let%transfo seq_for_to_magicthread_for ?(barrier_mark: mark = "") (tg: target) =
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    let seq_ind, p = try (
      let ind, seq_p = Path.index_in_seq p in
      Some ind, seq_p)
    with
    | e -> None, p in
    Target.apply_at_path (fun t ->
      let barrier = trm_add_mark barrier_mark (Barrier_trm.magic_barrier ()) in
      match seq_ind with
      | Some ind ->
        let instrs, ret = trm_inv ~error:"expected seq" trm_seq_inv t in
        let t = Mlist.nth instrs ind in
        let instrs = Mlist.replace_at ind (Loop_core.change_loop_mode_on MagicThread t) instrs in
        let instrs = match (Option.bind (Mlist.nth_opt instrs (ind + 1)) Barrier_trm.magic_barrier_inv) with
        | Some _ -> Mlist.replace_at (ind+1) barrier instrs
        | _ -> Mlist.insert_at (ind+1) barrier instrs in
        trm_alter ~desc:(Trm_seq (instrs,ret)) t
      | _ ->
        trm_seq (Mlist.of_list [
          Loop_core.change_loop_mode_on MagicThread t;
          barrier
        ])
    ) p
  ) tg)














type 'a op_handler = (var * 'a) -> (trm list) -> trm
type 'a memory_spec = {
  alloc_handler: (var * trm * trm * trms * bool) -> (trm * 'a * var);
  get_handler: 'a op_handler;
  set_handler: 'a op_handler;
  free_handler: 'a op_handler;
  cell_var: var;
  extra_patterns: (var * 'a) -> (typ -> typ) -> (typ -> unit -> typ) list
}

(* Global memory specification *)
let var_gmem = toplevel_var "GMem"
let var__gmem_get = toplevel_var "__gmem_get"
let var__gmem_set = toplevel_var "__gmem_set"
let var_gmem_free = toplevel_var "gmem_free"

let var__gmem_malloc nb_dims =
  toplevel_var (sprintf "__gmem_malloc%d" nb_dims)

let var_memcpy_host_to_device nb_dims =
  toplevel_var (sprintf "memcpy_host_to_device%d" nb_dims)

let var_memcpy_device_to_host nb_dims =
  toplevel_var (sprintf "memcpy_device_to_host%d" nb_dims)

let gmem_spec : unit memory_spec = {
  alloc_handler = (fun (array_var, typ_array, typ_alloc, trms, init) -> (
    assert (not init); (* TODO implement CALLOC *)
    let f = trm_add_cstyle (Typ_arguments [typ_alloc]) (trm_var (var__gmem_malloc (List.length trms))) in
    let alloc_instr = trm_apps f trms ~ghost_args:[(new_var "T", typ_alloc)] in
    (trm_let (array_var,typ_array) alloc_instr), (), array_var
  ));
  get_handler = (fun _ args -> trm_apps (trm_var var__gmem_get) args);
  set_handler = (fun _ args -> trm_apps (trm_var var__gmem_set) args);
  free_handler = (fun _ args -> trm_apps (trm_var var_gmem_free) args);
  cell_var = var_gmem;
  extra_patterns = (fun (var,_) aux -> [
    Pattern.(trm_apps (trm_var !__) !__ __ __) (fun v args () ->
      if (String.starts_with ~prefix:(sprintf "MATRIX%d_COPY_" (List.length args - 2)) v.name) then (
        let var_ind = List.find_index (fun arg ->
          match trm_var_inv arg with
            | Some v2 when (var_eq var v2) -> true
            | _ -> false) args in
          match var_ind with
          | Some 0 -> trm_apps (trm_var (var_memcpy_host_to_device (List.length args - 2))) args
          | Some 1 -> trm_apps (trm_var (var_memcpy_device_to_host (List.length args - 2))) args
          | _ -> raise_notrace Pattern.Next
        (*else if (String.starts_with ~prefix:(sprintf "MATRIX%d_MEMSET_" (List.length args)) v.name) then
          t *)
        ) else raise_notrace Pattern.Next
      );
  ]);
}

(* Shared memory specification *)

let var_smem = toplevel_var "SMem"
let var__smem_get = toplevel_var "__smem_get"
let var__smem_set = toplevel_var "__smem_set"

let var__smem_free nb_dims =
  toplevel_var (sprintf "__smem_free%d" nb_dims)

let var__smem_malloc nb_dims =
  toplevel_var (sprintf "__smem_malloc%d" nb_dims)

let desync_alloc_ghosts (inverse: bool) (distrib_dims: trm list) (real_dims: trm list) (matrix: var) (mem_typ: trm): trms =
  List.iter (fun dim -> Printf.printf "d %s\n" (Ast_to_c.ast_to_string dim)) distrib_dims;
  List.iter (fun dim -> Printf.printf "r %s\n" (Ast_to_c.ast_to_string dim)) real_dims;
  let open Resource_formula in
  let mul_nest dims =
    match dims with
    | h :: tl -> List.fold_right (fun t dim -> trm_mul_int dim t) tl h
    | _ -> failwith "expected 1 or more dimensions for mul nest" in

  let group_part distrib_ind distrib_dim =
    let real_inds = List.mapi (fun i _ -> new_var (sprintf "i%d" (i+1))) real_dims in
    let inside_formula = formula_uninit_cell ~mem_typ:mem_typ (
      Matrix_trm.access (trm_var matrix) (distrib_dim :: real_dims) (distrib_ind :: (List.map trm_var real_inds))
    ) in
    List.fold_right2 (fun idx dim formula ->
    trm_apps ~annot:formula_annot trm_group [formula_range (trm_int 0) dim (trm_int 1); formula_fun [idx, typ_int] formula])
    real_inds real_dims inside_formula in

  let wrap_desyncgroups distrib_inds distrib_dims inside_formula =
    List.fold_right2 (fun idx dim formula ->
    trm_apps ~annot:formula_annot trm_desyncgroup [dim; formula_fun [idx, typ_int] formula])
    distrib_inds distrib_dims inside_formula in

  let from, into = if inverse then
    ((mul_nest distrib_dims),(Matrix_trm.msize distrib_dims))
  else
    ((Matrix_trm.msize distrib_dims),(mul_nest distrib_dims)) in

  let msize_assume,msize_rewrite =
    let assume_msize_mult = Resource_trm.assume
      (Resource_formula.formula_is_true (trm_eq ~typ:typ_int from into)) in
    let distrib_ind = new_var "i" in
    let distrib_sz = new_var "sz" in
    let formula = formula_fun [distrib_sz, typ_int] (
      wrap_desyncgroups [distrib_ind] [trm_var distrib_sz] (
        group_part (trm_var distrib_ind) (trm_var distrib_sz)
      )
    ) in
    let msize_to_from_mult = Resource_trm.(ghost
      (ghost_rewrite_linear ~typ:typ_int ~into ~from
      formula)) in
    assume_msize_mult, msize_to_from_mult in

  let desync_tile_ghost_f = if inverse then ghost_desync_untile_divides_trivial else ghost_desync_tile_divides_trivial in
  let dmindex_tile_ghost_f = if inverse then ghost_dmindex_tile else ghost_dmindex_untile in
  let ghosts,_,_ = List.fold_left (fun (ghosts,remain_dims, seen_dims) dim ->
    match remain_dims with
    | _ :: [] ->
      (ghosts, [], seen_dims)
    | _ :: remain_dims' ->
      let dmindex_dims = (mul_nest remain_dims) :: seen_dims in
      let distrib_inds = List.mapi (fun i _ -> new_var (sprintf "di%d" (i+1))) dmindex_dims in
      let dmindex_expr = Matrix_trm.dmindex dmindex_dims (List.map trm_var distrib_inds) in
      let tile_items = wrap_desyncgroups distrib_inds dmindex_dims (
        group_part dmindex_expr (mul_nest distrib_dims)
      ) in
      let desync_tile_ghost = desync_tile_ghost_f tile_items dim (mul_nest remain_dims') in

      let dmindex_dims = (mul_nest remain_dims') :: dim :: seen_dims in
      let distrib_inds = List.mapi (fun i _ -> new_var (sprintf "di%d" (i+1))) dmindex_dims in
      let f = new_var "f" in
      let tile_items = wrap_desyncgroups distrib_inds dmindex_dims (
        group_part (formula_fun [f, typ_pure_simple_fun (List.map (fun _ -> typ_int) distrib_inds) typ_int]
          (trm_apps (trm_var f) (List.map trm_var distrib_inds)))
        (mul_nest distrib_dims)
      ) in
      let dmindex_tile_ghost = dmindex_tile_ghost_f dmindex_dims tile_items in

      (dmindex_tile_ghost::desync_tile_ghost::ghosts, remain_dims', dim :: seen_dims)
    | [] -> (ghosts, [], seen_dims)) ([], distrib_dims, []) distrib_dims in
  let ghosts = ghosts @ [msize_rewrite] in
  let ghosts = if inverse then ghosts else (List.rev ghosts) in
  msize_assume :: ghosts

let smem_spec : int -> (trm list * trm list) memory_spec = fun chop_dims -> {
  alloc_handler = (fun (array_var, typ_array, typ_alloc, trms, init) -> (
    assert (not init); (* TODO implement CALLOC *)
    let distrib_dims, real_dims = List.split_at chop_dims trms in
    let f = trm_add_cstyle (Typ_arguments [typ_alloc]) (trm_var (var__smem_malloc (List.length real_dims))) in
    let alloc_instr = trm_apps f real_dims ~ghost_args:[(new_var "T", typ_alloc)] in
    let t = trm_seq_nobrace (Mlist.of_list (
      (trm_let (array_var,typ_array) alloc_instr) ::
      (desync_alloc_ghosts false distrib_dims real_dims array_var (trm_var var_smem))
    )) in
    t, (distrib_dims,real_dims), array_var
  ));
  get_handler = (fun _ args -> trm_apps (trm_var var__smem_get) args);
  set_handler = (fun _ args -> trm_apps (trm_var var__smem_set) args);
  free_handler = (fun _ args -> trm_apps (trm_var (var__smem_malloc (List.length args - 1))) args);
  cell_var = var_smem;
  extra_patterns = (fun (var,_) aux -> []);
}


let convert_memory (spec: 'a memory_spec) (alloc_tg: target): unit =
  let open Resource_formula in
  let rec convert_cell_mem_type f =
    Pattern.pattern_match f [
      Pattern.(trm_cell __) (fun () -> (trm_cell ~mem_typ:(trm_var spec.cell_var) ()));
      Pattern.(trm_uninit_cell __) (fun () -> (trm_uninit_cell ~mem_typ:(trm_var spec.cell_var) ()));
      Pattern.__ (fun () -> trm_map convert_cell_mem_type f)
    ] in
  let rec convert_ops_mem_type alloc_data t =
    let var, aux_alloc_data = alloc_data in
    let aux = convert_ops_mem_type alloc_data in
    let t = trm_map aux t in
    Pattern.pattern_match t
    ((spec.extra_patterns alloc_data aux) @
    [
      Pattern.(trm_get !__) (fun addr () ->
        Pattern.when_ (is_free_var_in_trm var addr);
        spec.get_handler alloc_data [addr]
      );
      Pattern.(trm_set !__ !__) (fun addr rhs () ->
        Pattern.when_ (is_free_var_in_trm var addr);
        spec.set_handler alloc_data [addr;rhs]
      );
      Pattern.(formula_repr !__ !__ ) (fun l r () ->
        Pattern.when_ (is_free_var_in_trm var l);
        Resource_formula.formula_repr l (convert_cell_mem_type r)
      );
      Pattern.(trm_delete !__) (fun addr () ->
        Pattern.when_ (is_free_var_in_trm var addr);
        spec.free_handler alloc_data [addr]
      );
      Pattern.__ (fun () -> t)
    ]) in
  Target.apply_at_target_paths_in_seq (fun ind seq ->
     Nobrace.remove_after_trm_op (fun seq ->
      let instrs, ret = trm_inv ~error:"expected seq" trm_seq_inv seq in
      let alloc_trm = Mlist.nth instrs ind in
      let alloc_trm, aux_alloc_data, var = match (Matrix_trm.let_alloc_inv alloc_trm) with
      | Some alloc_specs -> spec.alloc_handler alloc_specs
      | _ -> failwith "Gpu_basic.ml: expected target to point to a matrix allocation" in
      let instrs = Mlist.replace_at ind alloc_trm instrs in
      seq
      |> (trm_alter ~desc:(Trm_seq (instrs, ret)))
      |> (convert_ops_mem_type (var,aux_alloc_data))
     ) seq
  ) alloc_tg



