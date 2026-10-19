open Prelude
open Target
open Flags

include Gpu_basic

(** [convert_tail_thread_for loops leaf] given a target [leaf] to a loop which is in tail position, convert len([loops]) number of loops
  (also in tail position, except for the last) surrounding it to thread for.
  A loop is in tail position if the only thing proceeding it is a ghost or the end of a surrounding loop.
  The list [loops] is a list containing either 0 or 1: 0 means skip conversion (leave it as sequential for), 1 means convert.
  It is always assumed that the leaf will be converted.
  The shared reads of the crossed loops are turned into exclusive reads that are desynchronized by the loops, and resynchronized
  by the barrier inserted after the outermost converted loop. *)
let%transfo convert_tail_thread_for (loops : int list) (leaf: target) =
  let fission_helper tg = Loop.fission tg in
  Target.iter (fun leaf_p -> Marks.with_marks (fun next_m ->
    let barrier_mark = next_m () in
    let rec nest_paths p n =
      if n = 0 then [p] else p :: nest_paths (snd (Path.index_in_surrounding_loop p)) (n - 1) in
    let loop_marks = List.map (Marks.add_next_mark_on next_m) (nest_paths leaf_p (List.length loops)) in
    (* Reads are made exclusive from the outermost loop to the leaf, so that each fork takes the fraction of the enclosing iteration. *)
    let join_marks = List.fold_left (fun join_marks loop_mark ->
      let join_mark = next_m () in
      Resources.shared_to_exclusive_reads ~mark_join:join_mark [cMark loop_mark];
      join_mark :: join_marks) [] (List.rev loop_marks) in
    let rec aux loops_incl_leaf join_marks leaf_p: unit =
      match loops_incl_leaf, join_marks with
      | convert :: loops, join_mark :: join_marks ->
        let leaf_t = target_of_path leaf_p in
        begin match convert with
        | 1 -> seq_for_to_magicthread_for ~barrier_mark leaf_t
        | 0 -> ()
        | _ -> failwith "Gpu.convert_tail_thread_for: loops should contain only 0 or 1"
        end;
        desync_forked_reads ~join_mark ~barrier_mark leaf_t;
        if loops <> [] then begin
          fission_helper [tBefore; cMark barrier_mark];
          (try
            remove_loop_around_barrier [cMark barrier_mark];
          with
          | e ->
            fission_helper [tAfter; cMark barrier_mark];
            remove_loop_around_barrier [cMark barrier_mark];
          );
          let _, next_leaf_p = Path.index_in_surrounding_loop leaf_p in
          aux loops join_marks next_leaf_p
        end
      | _ -> ()
    in
    aux (1 :: loops) join_marks (Target.resolve_mark_exactly_one (List.hd loop_marks))
  )) leaf

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
    | MagicThread -> Loop_core.change_loop_mode_on GpuThread t
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
