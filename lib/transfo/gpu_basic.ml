open Prelude
open Target
open Flags
open Gpu_trm

(* ----------------------------- Helpers ----------------------------- *)

let mul_nest dims =
    match dims with
    | h :: tl -> List.fold_left trm_mul_int h tl
    | _ -> failwith "expected 1 or more dimensions for mul nest"

let memtype_of_formula (f: formula) =
  let open Resource_formula in
  let cell = ref None in
  let rec aux f = Pattern.pattern_match f [
    Pattern.(formula_cell __ (trm_var !__)) (fun v () -> cell := Some v);
    Pattern.(formula_uninit_cell __ (trm_var !__)) (fun v () -> cell := Some v);
    Pattern.__ (fun () -> trm_iter aux f)
  ] in
  aux f;
  !cell

(* -------------------------- Kernel launches ------------------------ *)

let ghost_var_take_smem_token = toplevel_var "take_smem_token"
let ghost_var_give_smem_token = toplevel_var "give_smem_token"

let ghost_var_kernel_teardown_sync = toplevel_var "kernel_teardown_sync"

(* take 4 targets, one for each transition point. If they don't all belong to the same sequence,
  the transfo will complain. Since they belong to the same sequence, we know we can lift those instrs out into their own
  sequence, to make the printers job easier. Or we could just make the printer's job easier by detecting the start within sequences.*)
(* TODO: take another target, which is the launching function, for modifying the contract to include hostctx and __requires for the kernel retiling. *)
(* TODO: cleaner to take a tuple of tpbs bpgs etc.? or a dedicated kernel launch type? less arguments *)
(* TODO: could consider using "hint marks" from the smem lift transfo to place setup and end properly  *)
(* TODO: belongs in gpu_basic ? *)
(* TODO: customizable mark for the kernel sequence *)
let%transfo create_kernel_launch ?(grid_override: trm list option) ?(setup_end: target option) ?(teardown_begin: target option) (bpgs: trm list) (tpbs: trm list) (smem_szs: trm list)
  (start: target) (stop: target): unit =
  (fun () -> (fun () -> (fun next_m ->
    let tpb = Matrix_trm.msize tpbs in
    let bpg = Matrix_trm.msize bpgs in
    let smem_sz = List.fold_right trm_add_int smem_szs (trm_int 0) in (* TODO *)
    let grid = match grid_override with
    | Some grid_override -> Matrix_trm.msize grid_override
    | _ -> Matrix_trm.msize (bpgs @ tpbs) in

    let launch_mark = next_m () in
    let kill_mark = next_m () in

    let launch_call = trm_apps (trm_var var_kernel_launch) [bpg;tpb;smem_sz] in
    let take_token_instrs = List.fold_right (fun tok_sz instrs ->
      (Resource_trm.ghost (ghost_call ghost_var_take_smem_token ["tok_sz", tok_sz])) :: instrs) smem_szs [] in
    let launch = trm_add_mark (launch_mark) (trm_seq_nobrace (Mlist.of_list (launch_call :: take_token_instrs))) in

    let setup_call = trm_apps (trm_var var_kernel_setup_end) [] ~ghost_args:[(new_var "grid_sz", grid)] in
    let setup_instrs = match grid_override with
      | Some _ -> [setup_call]
      | _ ->
        let assume_retile = Resource_trm.assume (Resource_formula.formula_is_true (trm_eq ~typ:typ_int (trm_mul_int bpg tpb) grid)) in
        [ assume_retile; setup_call ] in
    let setup = trm_seq_nobrace (Mlist.of_list setup_instrs) in

    let teardown = trm_apps (trm_var var_kernel_teardown_begin) [] ~ghost_args:[(new_var "grid_sz", grid)] in

    let kill_call = trm_apps (trm_var var_kernel_kill) [] in
    let give_token_instrs = List.fold_left (fun instrs tok_sz ->
      (Resource_trm.ghost (ghost_call ghost_var_give_smem_token ["tok_sz", tok_sz])) :: instrs) [kill_call] smem_szs in
    let kill = trm_add_mark kill_mark (trm_seq_nobrace (Mlist.of_list give_token_instrs)) in

    (* TODO: sanitize and tell the user each target should only have one occurence *)
    Sequence_basic.insert ~reparse:false launch start;
    Sequence_basic.insert ~reparse:false setup (Option.unsome_or_else setup_end (fun () -> [tAfter; cMark launch_mark]));
    Sequence_basic.insert ~reparse:false kill stop;
    Sequence_basic.insert ~reparse:false teardown (Option.unsome_or_else teardown_begin (fun () -> [tBefore; cMark kill_mark]));

    Sequence.intro_between ~mark:"kernel_sequence" [tBefore; cMark launch_mark] [tAfter; cMark kill_mark]
  ) |> Marks.with_marks) |> Nobrace_transfo.remove_after) |> Flags.with_flag Flags.recompute_resources_between_steps false


(* ------------------- Thread for conversion ------------------------- *)

(* TODO: document *)
let%transfo seq_for_to_magicthread_for ?(barrier_mark: mark = "") (tg: target) =
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    let seq_ind, p = try (
      let ind, seq_p = Path.index_in_seq p in
      Some ind, seq_p)
    with
    | e -> None, p in
    Target.apply_at_path (fun t ->
      let barrier = trm_add_mark barrier_mark (Gpu_trm.magic_barrier ()) in
      match seq_ind with
      | Some ind ->
        let instrs, ret = trm_inv ~error:"expected seq" trm_seq_inv t in
        let loop = Mlist.nth instrs ind in
        let instrs = Mlist.replace_at ind (Loop_core.change_loop_mode_on MagicThread loop) instrs in
        let instrs = match (Option.bind (Mlist.nth_opt instrs (ind + 1)) Gpu_trm.magic_barrier_inv) with
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

(* Patching thread for conversion *)
let ghost_var_rewrite_threadsctx_sz = toplevel_var "rewrite_threadsctx_sz"
let ghost_var_rewrite_threadsctx_sz1 = toplevel_var "rewrite_threadsctx_sz1"

let%transfo insert_threadsctx_rewrite ?(msize: bool = false) (from: trm) (into: trm) (tg: target) =
  let v = if (msize) then ghost_var_rewrite_threadsctx_sz1 else ghost_var_rewrite_threadsctx_sz in
  let ghost = Resource_trm.ghost (ghost_call v ["from",from;"to",into]) in
  Sequence_basic.insert ~reparse:false ghost tg

(* ----------------------- Memory conversion ------------------------- *)

(* DesyncGroups and DMINDEX ghosts *)

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

let ghost_untile_divides_trivial items tile_count tile_size =
  Resource_trm.ghost (ghost_call (toplevel_var "untile_divides")
    ([("items", items);
    ("div_check", (trm_apps ~annot:Resource_formula.formula_annot (trm_var (toplevel_var "eq_refl")) [trm_mul_int tile_count tile_size]));
    ("tile_count", tile_count); ("tile_size", tile_size)]))

let ghost_dmindex_tile dims res_pattern =
  Resource_trm.ghost (ghost_call (toplevel_var (sprintf "dmindex%d_tile" (List.length dims))) (("H", res_pattern) :: List.mapi (fun i dim -> sprintf "n%d" (i+1), dim) dims))

let ghost_dmindex_untile dims res_pattern =
  Resource_trm.ghost (ghost_call (toplevel_var (sprintf "dmindex%d_untile" (List.length dims))) (("H", res_pattern) :: List.mapi (fun i dim -> sprintf "n%d" (i+1), dim) dims))

(* Memory specifications *)

type 'a op_handler = (var * 'a) -> (trm list) -> trm
type 'a memory_spec = {
  alloc_handler: trm -> (trm * 'a * var);
  get_handler: 'a op_handler;
  set_handler: 'a op_handler;
  free_handler: trm -> 'a op_handler;
  cell_var: var;
  extra_patterns: (var * 'a) -> (typ -> typ) -> (typ -> unit -> typ) list
}

(* TODO: make this a real %transfo? *)
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
      Pattern.(trm_compound_assign_any !__ !__ !__ !__) (fun typ op t1 t2 () ->
        Pattern.when_ ((is_free_var_in_trm var t1) || (is_free_var_in_trm var t2));
        convert_ops_mem_type alloc_data (trm_set t1 (trm_apps ~typ (trm_binop typ op) [(trm_get t1); t2]))
      );
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
        spec.free_handler t alloc_data [addr]
      );
      Pattern.__ (fun () -> t)
    ]) in
  Target.apply_at_target_paths_in_seq (fun ind seq ->
     Nobrace.remove_after_trm_op (fun seq ->
      let instrs, ret = trm_inv ~error:"expected seq" trm_seq_inv seq in
      let alloc_trm = Mlist.nth instrs ind in
      let alloc_trm, aux_alloc_data, var = spec.alloc_handler alloc_trm in
      let instrs = Mlist.replace_at ind alloc_trm instrs in
      seq
      |> (trm_alter ~desc:(Trm_seq (instrs, ret)))
      |> (convert_ops_mem_type (var,aux_alloc_data))
     ) seq
  ) alloc_tg

(* TODO: make this a real %transfo? *)
let fix_distrib_accesses ~(aliases: Var_set.t ref) (chop_dims: int) (body_span_tg: target) (alloc_tg: target): unit =
  let body_seq_path,body_seq_span = Target.resolve_target_span_exactly_one body_span_tg in
  let open Resource_formula in
  (* TODO wont work if alloc_tg points to more than one *)
  Target.iter_at_target_paths (fun alloc_trm ->
    let error = "Gpu_basic.ml: expected target to point to a matrix allocation" in
    let var, _, _ = trm_inv ~error trm_let_inv alloc_trm in

    let rec aux ?(alias_binder:var option) loop_depth t = match (Matrix_trm.access_inv t) with
    | Some ({desc = Trm_var base}, dims, inds) when (var_eq var base) ->
      (match alias_binder with
      | Some v -> aliases := Var_set.add v !aliases;
      | _ -> ());
      let distrib_dims, real_dims = List.split_at chop_dims dims in
      let distrib_inds, real_inds = List.split_at chop_dims inds in
      trm_array_access (trm_var base) (Matrix_trm.mindex ((mul_nest distrib_dims) :: real_dims)
        ((Matrix_trm.dmindex distrib_dims distrib_inds) :: real_inds))
    | _ ->
      Pattern.pattern_match t [
        Pattern.(trm_let !__ __ __ ) (fun v () -> trm_map (aux ~alias_binder:v loop_depth) t);
        Pattern.(trm_for __ __ __ __) (fun () -> trm_map (aux (loop_depth + 1)) t);
        (* LATER: better heuristics to convert the desyncgroups if ghosts are being used on these dimensions *)
        Pattern.(formula_group !__ !(formula_range __ !__ __) !__) (fun ind range stop body () ->
          Pattern.when_ (is_free_var_in_trm var t);
          let body = aux (loop_depth + 1) body in
          if (chop_dims - loop_depth <= 0) then
            formula_group ind range body
          else
            formula_desyncgroup ind stop body
        );
        Pattern.(formula_desyncgroup !__ !__ !__) (fun ind stop body () ->
          formula_desyncgroup ind stop (aux (loop_depth + 1) body)
        );
        Pattern.__ (fun () -> trm_map (aux loop_depth) t)
      ] in
    Target.apply_at_path (fun body_seq ->
      update_span_helper body_seq_span body_seq (fun instrs ->
      Mlist.to_list (Mlist.map (fun instr -> Trm (aux 0 instr)) instrs))
    ) body_seq_path) alloc_tg

let desync_alloc_ghosts (inverse: bool) (distrib_dims: trm list) (real_dims: trm list) (matrix: var) (mem_typ: trm): trms =
  let open Resource_formula in

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
    if (inverse) then
      (* when free-ing, we expect things to already be synchronized (groups) *)
      trm_apps ~annot:formula_annot trm_group [formula_range (trm_int 0) dim (trm_int 1); formula_fun [idx, typ_int] formula]
    else
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
        group_part (Matrix_trm.dmindex [trm_var distrib_sz] [trm_var distrib_ind]) (trm_var distrib_sz)
      )
    ) in
    let msize_to_from_mult = Resource_trm.(ghost
      (ghost_rewrite_linear ~typ:typ_int ~into ~from
      formula)) in
    assume_msize_mult, msize_to_from_mult in

  let desync_tile_ghost_f = if inverse then ghost_untile_divides_trivial else ghost_desync_tile_divides_trivial in
  let dmindex_tile_ghost_f = if inverse then ghost_dmindex_tile else ghost_dmindex_untile in
  let rec make_ghosts ghosts seen_dims remain_dims =
    let ghosts = match remain_dims with
    | [] -> ghosts
    | (__ :: []) -> ghosts
    | dim :: remain_dims' ->
      let remain_dims' = List.rev remain_dims' in
      let tile_dim = (mul_nest (remain_dims' @ [dim])) in
      let tile_ind = new_var "di" in
      let distrib_inds = List.mapi (fun i _ -> new_var (sprintf "di%d" (i+1))) seen_dims in
      let dmindex_expr = Matrix_trm.dmindex (tile_dim :: seen_dims) (List.map trm_var (tile_ind :: distrib_inds)) in
      let tile_items = wrap_desyncgroups distrib_inds seen_dims (
        group_part dmindex_expr (mul_nest distrib_dims)
      ) in
      let tile_items = formula_fun [tile_ind, typ_int] tile_items in
      let desync_tile_ghost = desync_tile_ghost_f tile_items (mul_nest remain_dims') dim in

      let dmindex_dims = (mul_nest remain_dims') :: dim :: seen_dims in
      let distrib_inds = List.mapi (fun i _ -> new_var (sprintf "di%d" (i+1))) dmindex_dims in
      let f = new_var "f" in
      let tile_items = (formula_fun [f, typ_pure_simple_fun (List.map (fun _ -> typ_int) distrib_inds) typ_int]
      (wrap_desyncgroups distrib_inds dmindex_dims (
        group_part (trm_apps (trm_var f) (List.map trm_var distrib_inds)) (mul_nest distrib_dims))
      )) in
      let dmindex_tile_ghost = dmindex_tile_ghost_f dmindex_dims tile_items in

      desync_tile_ghost::dmindex_tile_ghost::ghosts in
    match seen_dims with
    | h :: tl -> make_ghosts ghosts tl (h :: remain_dims)
    | _ -> ghosts in

  let ghosts = make_ghosts [] distrib_dims [] in
  let ghosts = msize_rewrite :: ghosts in
  let ghosts = if inverse then (List.rev ghosts) else ghosts in
  msize_assume :: ghosts

(* Global memory specification *)

let gmem_spec : unit memory_spec = {
  alloc_handler = (fun alloc_trm -> (
    let error = "Gpu.convert_to_global_mem: expected target to point to a matrix allocation" in
    let (array_var, typ_array, typ_alloc, trms, init) = trm_inv ~error (Matrix_trm.let_alloc_inv) alloc_trm in
    assert (not init); (* TODO implement CALLOC *)
    let f = trm_add_cstyle (Typ_arguments [typ_alloc]) (trm_var (var__gmem_malloc (List.length trms))) in
    let alloc_instr = trm_apps f trms ~ghost_args:[(new_var "T", typ_alloc)] in
    (trm_let (array_var,typ_array) alloc_instr), (), array_var
  ));
  get_handler = (fun _ args -> trm_apps (trm_var var__gmem_get) args);
  set_handler = (fun _ args -> trm_apps (trm_var var__gmem_set) args);
  free_handler = (fun _ _ args -> trm_apps (trm_var var_gmem_free) args);
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

let smem_spec ?(alloc_mark="") ?(free_mark="") (chop_dims: int): (trm list * trm list) memory_spec = {
  alloc_handler = (fun alloc_trm -> (
    let error = "Gpu.convert_to_global_mem: expected target to point to a matrix allocation" in
    let (array_var, typ_array, typ_alloc, trms, init) = trm_inv ~error (Matrix_trm.let_alloc_inv) alloc_trm in
    assert (not init); (* LATER: implement CALLOC *)
    let distrib_dims, real_dims = List.split_at chop_dims trms in
    let f = trm_add_cstyle (Typ_arguments [typ_alloc]) (trm_var (var__smem_malloc (List.length real_dims))) in
    let alloc_instr = trm_apps f real_dims ~ghost_args:[(new_var "T", typ_alloc)] in
    let instrs = Mlist.of_list (
      (trm_let (array_var,typ_array) alloc_instr) ::
      (desync_alloc_ghosts false distrib_dims real_dims array_var (trm_var var_smem))
    ) in
    let t = trm_seq_nobrace (Mlist.insert_mark_at (Mlist.length instrs) alloc_mark instrs) in
    t, (distrib_dims,real_dims), array_var
  ));
  get_handler = (fun _ args -> trm_apps (trm_var var__smem_get) args);
  set_handler = (fun _ args -> trm_apps (trm_var var__smem_set) args);
  free_handler = (fun _ (var, (distrib_dims, real_dims)) args ->
    let free_instr = trm_apps (trm_var (var__smem_free (List.length real_dims))) (args @ real_dims) in
    let instrs = (Mlist.of_list
      ((desync_alloc_ghosts true distrib_dims real_dims var (trm_var var_smem)) @ [free_instr])) in
    trm_seq_nobrace (Mlist.insert_mark_at 0 free_mark instrs)
  );
  cell_var = var_smem;
  extra_patterns = (fun (var,_) aux -> []);
}

let smem_alias_spec (alias_var: var): unit memory_spec = {
  alloc_handler = (fun t -> (t,(),alias_var));
  get_handler = (fun _ args -> trm_apps (trm_var var__smem_get) args);
  set_handler = (fun _ args -> trm_apps (trm_var var__smem_set) args);
  free_handler = (fun t _ _ -> t);
  cell_var = var_smem;
  extra_patterns = (fun (var,_) aux -> []);
}

(* ----------------------- Barrier conversion ------------------------ *)

let block_sync (t: trm): trm =
  trm_apps ~ghost_args:[new_var "H", t] (trm_var (toplevel_var "blocksync")) []

let kernel_teardown_sync (t: trm): trm =
  Resource_trm.ghost (ghost_call (toplevel_var "kernel_teardown_sync") ["H", t])

let is_smem_or_gmem (f: formula): bool =
  match (memtype_of_formula f) with
  | Some v when (var_eq v var_smem) -> true
  | Some v when (var_eq v var_gmem) -> true
  | _ -> false

(* LATER: generic transfos for barrier conversion (take the barrier desired as argument )*)
let%transfo magic_barrier_to_blocksync (kernel_body: target) (tg: target): unit =
  (* TODO: blocksync breaks the __strict() loop contracts, because it asks
    for a fraction of the kernel_params. Ideally, we would not need to have the target to the kernel_body. *)
  Resources.with_non_strict_loop_contracts ([nbAny] @ kernel_body @ [cFor ""]) (fun () ->
    Target.iter (fun p ->
      Resources.ensure_computed_at p;
      Target.apply_at_path (Gpu_trm.magic_barrier_to_seq block_sync is_smem_or_gmem) p) tg
  )

let%transfo magic_barrier_to_teardown_sync (tg: target): unit =
  Target.iter (fun p ->
    Resources.ensure_computed_at p;
    Target.apply_at_path (Gpu_trm.magic_barrier_to_seq kernel_teardown_sync is_smem_or_gmem) p)
  tg

