open Prelude
open Target
open Flags

(* Launch-related variables *)

let var_kernel_launch = toplevel_var "kernel_launch"
let var_kernel_setup_end = toplevel_var "kernel_setup_end"
let var_kernel_teardown_begin = toplevel_var "kernel_teardown_begin"
let var_kernel_kill = toplevel_var "kernel_kill"
let ghost_var_kernel_teardown_sync = toplevel_var "kernel_teardown_sync"

(* GPU-specific memory variables *)

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

let ghost_group_to_desyncgroup = toplevel_var "group_to_desyncgroup"

let convert_to_desyncgroup_ghost (n: formula) (items: formula): trm =
  Resource_trm.ghost (ghost_call ghost_group_to_desyncgroup [
    "N", n; "items", items
  ])

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




























let%transfo convert_to_global_mem ~(var: string) (tg: target): unit =
  let open Resource_formula in
  let rec convert_cell_mem_type f =
    Pattern.pattern_match f [
      Pattern.(trm_cell __) (fun () -> (trm_cell ~mem_typ:(trm_var var_gmem) ()));
      Pattern.(trm_uninit_cell __) (fun () -> (trm_uninit_cell ~mem_typ:(trm_var var_gmem) ()));
      Pattern.__ (fun () -> trm_map convert_cell_mem_type f)
    ] in
  let (var, _) = find_var var tg in
  let rec convert_ops_mem_type t =
    (* let override = match (trm_let_inv t) with
    | Some (let_var,_,_) when (var_eq let_var var) -> true
    | _ -> override in *)
    (* small optimization: dont do search for variable in every term (O(n^2)), cache search result for subterms
      although maybe its not useful because we dont care about all subterms *)
    let t = trm_map convert_ops_mem_type t in
    Pattern.pattern_match t [
      Pattern.(trm_get !__) (fun addr () ->
        Pattern.when_ (is_free_var_in_trm var addr);
        trm_apps (trm_var var__gmem_get) [addr]
      );
      Pattern.(trm_set !__ !__) (fun addr rhs () ->
        Pattern.when_ (is_free_var_in_trm var addr);
        trm_apps (trm_var var__gmem_set) [addr; rhs]
      );
      Pattern.(formula_repr !__ !__ ) (fun l r () ->
        Pattern.when_ (is_free_var_in_trm var l);
        Resource_formula.formula_repr l (convert_cell_mem_type r)
      );
      Pattern.(trm_delete !__) (fun addr () ->
        Pattern.when_ (is_free_var_in_trm var addr);
        trm_apps (trm_var var_gmem_free) [addr]
      );
      (* !(check (fun v -> String.starts_with ~prefix:"MATRIX" v.name)) *)
      Pattern.(trm_apps (trm_var !__) !__ __ __) (fun v args () ->
        if (String.starts_with ~prefix:(sprintf "MATRIX%d_COPY_" (List.length args - 2)) v.name) then (
          let var_ind = List.find_index (fun arg ->
            match trm_var_inv arg with
            | Some v2 when (var_eq var v2) -> true
            | _ -> false) args in
          match var_ind with
          | Some 0 -> trm_apps (trm_var (var_memcpy_host_to_device (List.length args - 2))) args
          | Some 1 -> trm_apps (trm_var (var_memcpy_device_to_host (List.length args - 2))) args
          | _ -> raise Pattern.Next
        (*else if (String.starts_with ~prefix:(sprintf "MATRIX%d_MEMSET_" (List.length args)) v.name) then
          t *)
        ) else raise Pattern.Next
      );
      Pattern.__ (fun () ->
        match (Matrix_trm.let_alloc_inv t) with
        | Some (array_var, typ_array, typ_alloc, trms, init) when (var_eq array_var var) ->
          assert (not init); (* TODO implement CALLOC *)
          let f = trm_add_cstyle (Typ_arguments [typ_alloc]) (trm_var (var__gmem_malloc (List.length trms))) in
          let alloc_instr = trm_apps f trms ~ghost_args:[(new_var "T", typ_alloc)] in
          trm_let (var,typ_array) alloc_instr
        | _ -> t )
    ] in
  Target.apply_at_target_paths (fun t -> convert_ops_mem_type t) tg


(* TODO: support more than kernel teardown sync *)

(* let%transfo convert_magic_sync (tg: target): unit =
  Target.apply_at_target_paths (fun t ->
    match t.desc with
    | Trm_apps ({desc = Trm_var var_magic_sync}, tl, ga, gb) ->
      trm_add_attribute GhostInstr (trm_alter ~desc:(Trm_apps (trm_var ghost_var_kernel_teardown_sync, tl, ga, gb)) t)
    | _ -> failwith "Gpu_basic.convert_magic_sync: expected target to point to a magic sync instruction.") tg *)
