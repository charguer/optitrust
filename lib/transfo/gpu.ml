open Prelude
open Target
open Flags

include Gpu_basic

let seq_for_desync_parallel_reads_on ~(barrier_mark : mark) (t: trm): trm =
  let (range, mode, body, contract) = trm_inv ~error:"expected for loop" trm_for_inv t in
  let forked_reads = List.map (fun (h, f) ->
    let open Resource_formula in
    let { frac; formula } = trm_inv ~error:"expected read only" formula_read_only_inv f in
    (new_anon_hyp (), formula_desync_ro frac formula)
  ) contract.parallel_reads in
  let new_contract = { contract with
    parallel_reads = [];
    iter_contract = {
      pre = Resource_set.add_linear_list forked_reads contract.iter_contract.pre;
      post = Resource_set.add_linear_list contract.parallel_reads contract.iter_contract.post
    }
  } in
  let forkjoins = List.map (fun (h, f) ->
    let open Resource_formula in
    let open Resource_trm in
    let { frac; formula } = trm_inv ~error:"expected read only" formula_read_only_inv f in
    let range_formula = formula_loop_range range in
    (ghost (ghost_dro_fork_group ~range:range_formula formula),
     ghost (ghost_ro_join_group ~range:range_formula formula))
  ) contract.parallel_reads in
  let (forks, joins) = List.split forkjoins in
  trm_seq_helper ~braces:false [
    TrmList forks;
    Trm (trm_for ~annot:t.annot ~mode ~contract:new_contract range body);
    Trm (trm_add_mark barrier_mark (Gpu_trm.magic_barrier ()));
    TrmList joins;
  ]

let%transfo seq_for_desync_parallel_reads ~(barrier_mark : mark) (tg: target): unit =
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    Target.apply_at_path (seq_for_desync_parallel_reads_on ~barrier_mark) p
  ) tg)

(** [convert_tail_thread_for loops leaf] given a target [leaf] to a loop which is in tail position, convert len([loops]) number of loops
  (also in tail position, except for the last) surrounding it to thread for.
  A loop is in tail position if the only thing proceeding it is a ghost or the end of a surrounding loop.
  The list [loops] is a list containing either 0 or 1: 0 means skip conversion (leave it as sequential for), 1 means convert.
  It is always assumed that the leaf will be converted. *)
let%transfo convert_tail_thread_for (loops : int list) (leaf: target) =
  Marks.with_marks (fun next_m -> Nobrace_transfo.remove_after (fun () ->
    let rec aux ~(is_leaf: bool) barrier_mark loops_incl_leaf leaf_p: unit =
      let convert,loops = match loops_incl_leaf with
      | 0 :: tl -> false, tl
      | 1 :: tl -> true, tl
      | _ -> failwith "Gpu.convert_tail_thread_for: loops should contain only 0 or 1" in
      let leaf_m = Marks.add_next_mark_on next_m leaf_p in
      let new_barrier_mark = next_m () in
      let ro_join_barrier_mark = next_m () in
      if not is_leaf then begin
        seq_for_desync_parallel_reads ~barrier_mark:new_barrier_mark [cMark leaf_m];
        Loop.fission ~mark_loops:leaf_m [tBefore; cMark barrier_mark];
        Loop.fission ~mark_loops:ro_join_barrier_mark [tAfter; cMark barrier_mark];
        Instr.delete [cMark leaf_m; cFor ~body:[cMark barrier_mark] ""];
        Instr.move ~dest:[tAfter; cMark new_barrier_mark] [nbAny; cMark ro_join_barrier_mark];
      end;
      if convert then seq_for_to_magicthread_for ~one_thread_inside:is_leaf ~barrier_mark:new_barrier_mark [occFirst; cMark leaf_m];
      match loops with
      | [] -> ()
        (* Target.apply_at_path (fun t ->
          trm_seq_nobrace_nomarks [t; Gpu_trm.magic_barrier ()]
        ) leaf_p *)
      | _ ->
        let _, next_leaf_p = Path.index_in_surrounding_loop leaf_p in
        aux ~is_leaf:false new_barrier_mark loops next_leaf_p
    in
    Target.iter (aux ~is_leaf:true no_mark (1 :: loops)) leaf;
  ))

(** [convert_magic_thread_fors ~patch_steps tg]: convert all loops pointed to by [tg] to thread for from magic thread for.
  [tg] should only point to loops, but it is OK if it points to loops that are not magic thread for, it will just skip those.
  For example, you can probably use something like [nbAny; cFor "outermost_block_loop"; cFor] most of the time.
  If supplied, [~patch_steps] is called before recomputing resources, this is useful if some ghosts for rewriting threadsctx need to be added. *)
let%transfo convert_magic_thread_fors ?(patch_steps: unit -> unit = fun () -> ()) (tg: target) =
  Flags.with_flag Flags.recompute_resources_between_steps false (fun () ->
  Target.apply_at_target_paths (fun t ->
    let error = "Gpu.convert_magic_thread_fors: expected a target to a simple for loop" in
    let range, mode, body, contract = trm_inv ~error trm_for_inv t in
    match mode with
    | MagicThread false -> Loop_core.change_loop_mode_on GpuThread t
    | MagicThread true -> Loop_core.change_loop_mode_on GpuThread (Resources.desyncro_to_ro_on t)
    | _ ->
      trm_alter t ~desc:(Trm_for (range, mode, body, { contract with strict = false}))
  ) tg;
  patch_steps ();
  Resources.ensure_computed ();
  Resources.make_strict_loop_contracts tg)

(** [convert_to_global_mem tg] convert the targeted declaration to global memory, replacing all operations on that variable with the appropriate ones. *)
let%transfo convert_to_global_mem (tg: target): unit =
  Target.iter (fun p ->
    let _,tg_seq_p = Path.index_in_seq p in
    Resources.with_non_strict_loop_contracts [cPath tg_seq_p] (fun () ->
      Gpu_basic.convert_memory Gpu_basic.gmem_spec [cPath p];
    )
  ) tg

(** [convert_to_shared_mem ~chop_dims tg] convert the targeted declaration to shared memory, replacing all operations on that variable with the appropriate ones.
  [chop_dims]: number of dimensions that will become distributed, that should be chopped off. E.g. If there are 2 block dimensions, then chop_dims should most likely just be 2. *)
let%transfo convert_to_shared_mem ~(chop_dims: int) (tg: target): unit =
  Target.iter (fun p ->
    let _,tg_seq_p = Path.index_in_seq p in
    let tg = [cPath p] in
    Marks.with_marks (fun next_m ->
        Resources.with_non_strict_loop_contracts [cPath tg_seq_p] (fun () ->
          let alloc_mark = next_m () in
          let free_mark = next_m () in
          Gpu_basic.convert_memory (Gpu_basic.smem_spec ~alloc_mark ~free_mark chop_dims) tg;
          let aliases = ref Var_set.empty in
          (* FIXME: support multiple kernels *)
          Gpu_basic.fix_distrib_accesses ~aliases ~synced:true chop_dims [tSpan [cMark alloc_mark] [tBefore; cCall "kernel_setup_end"]] tg;
          Gpu_basic.fix_distrib_accesses ~aliases chop_dims [tSpan [tAfter; cCall "kernel_setup_end"] [tBefore; cCall "kernel_teardown_begin"]] tg;
          Gpu_basic.fix_distrib_accesses ~aliases ~synced:true chop_dims [tSpan [tAfter; cCall "kernel_teardown_begin"] [cMark free_mark]] tg;
          Var_set.iter (fun alias ->
            Gpu_basic.convert_memory (Gpu_basic.smem_alias_spec alias) tg;
          ) !aliases;
  ))) tg

(** [convert_to_register_mem ~chop_dims tg] convert the targeted declaration to register memory, replacing all operations on that variable with the appropriate ones.
  [chop_dims]: number of dimensions that will become distributed, that should be chopped off. *)
let%transfo convert_to_register_mem ~(chop_dims: int) (tg: target): unit =
  Target.iter (fun p ->
    let _,tg_seq_p = Path.index_in_seq p in
    let tg = [cPath p] in
    Marks.with_marks (fun next_m ->
        Resources.with_non_strict_loop_contracts [cPath tg_seq_p] (fun () ->
          let alloc_mark = next_m () in
          let free_mark = next_m () in
          Gpu_basic.convert_memory (Gpu_basic.treg_mem_spec ~alloc_mark ~free_mark chop_dims) tg;
          let aliases = ref Var_set.empty in
          Gpu_basic.fix_distrib_accesses ~aliases chop_dims [tSpan [cMark alloc_mark] [cMark free_mark]] tg;
          (* TODO ?
          (* FIXME: support multiple kernels *)
          Gpu_basic.fix_distrib_accesses ~aliases ~synced:true chop_dims [tSpan [cMark alloc_mark] [tBefore; cCall "kernel_setup_end"]] tg;
          Gpu_basic.fix_distrib_accesses ~aliases chop_dims [tSpan [tAfter; cCall "kernel_setup_end"] [tBefore; cCall "kernel_teardown_begin"]] tg;
          Gpu_basic.fix_distrib_accesses ~aliases ~synced:true chop_dims [tSpan [tAfter; cCall "kernel_teardown_begin"] [cMark free_mark]] tg; *)
          Var_set.iter (fun alias ->
            Gpu_basic.convert_memory (Gpu_basic.treg_mem_alias_spec alias) tg;
          ) !aliases;
  ))) tg
