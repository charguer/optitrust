open Prelude
open Target
open Flags

include Gpu_basic

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

let%transfo convert_tail_thread_for (loops : int list) (leaf: target) =
  let fission_helper tg =
    Flags.with_flag Flags.check_validity true (fun () -> Loop.fission tg) in
  let rec aux barrier_mark loops_incl_leaf leaf_p: unit =
    let convert,loops = match loops_incl_leaf with
    | 0 :: tl -> false, tl
    | 1 :: tl -> true, tl
    | _ -> failwith "Gpu.convert_tail_thread_for: loops should contain only 0 or 1" in
    let leaf_t = (target_of_path leaf_p) in
    (* Don't want to have __xreads in a thread for because you can't resynchronize fractions. *)
    Resources.detach_loop_ro_focus leaf_t;
    if convert then
      seq_for_to_magicthread_for ~barrier_mark leaf_t;
    (* assume the barrier mark is already there.. *)
    (* push_work_before [cMark barrier_mark] ... *)
    match loops with
    | [] -> ()
    | _ ->
      fission_helper [tBefore; cMark barrier_mark];
      (try
        Barriers.remove_loop_around_barrier [cMark barrier_mark];
      with
      | e ->
        fission_helper [tAfter; cMark barrier_mark];
        Barriers.remove_loop_around_barrier [cMark barrier_mark];
      );
      let _, next_leaf_p = Path.index_in_surrounding_loop leaf_p in
      aux barrier_mark loops next_leaf_p
    in
  Marks.with_marks (fun next_m ->
    Target.iter (aux (next_m ()) (1 :: loops)) leaf;
  )

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

let%transfo convert_to_global_mem (tg: target): unit =
  Target.iter (fun p ->
    let _,tg_seq_p = Path.index_in_seq p in
    Resources.with_non_strict_loop_contracts [cPath tg_seq_p] (fun () ->
      Gpu_basic.convert_memory Gpu_basic.gmem_spec [cPath p]
    )
  ) tg


let%transfo convert_to_shared_mem ~(chop_dims: int) (tg: target): unit =
  Target.iter (fun p ->
    let _,tg_seq_p = Path.index_in_seq p in
    let tg = [cPath p] in
    Marks.with_marks (fun next_m ->
      Trace.without_resource_computation_between_steps (fun () ->
        Resources.with_non_strict_loop_contracts [cPath tg_seq_p] (fun () ->
          let alloc_mark = next_m () in
          let free_mark = next_m () in
          Gpu_basic.convert_memory (Gpu_basic.smem_spec ~alloc_mark ~free_mark chop_dims) tg;
          let aliases = ref Var_set.empty in
          let kernel_seq = [tSpan [cMark alloc_mark] [cMark free_mark]] in
          Gpu_basic.fix_distrib_accesses ~aliases chop_dims kernel_seq tg;
          Var_set.iter (fun alias ->
            Gpu_basic.convert_memory (Gpu_basic.smem_alias_spec alias) tg;
          ) !aliases;
  )))) tg

let%transfo magic_barrier_to_blocksync (kernel_body: target) (tg: target): unit =
  (* TODO: blocksync breaks the __strict() loop contracts, because it asks
    for a fraction of the kernel_params. Ideally, we would not need to have the target to the kernel_body. *)
  Resources.with_non_strict_loop_contracts ([nbAny] @ kernel_body @ [cFor ""]) (fun () ->
    Target.iter (fun p ->
      Resources.ensure_computed_at p;
      Target.apply_at_path (Barrier_trm.magic_barrier_to_seq block_sync is_smem_or_gmem) p) tg
  )

let%transfo magic_barrier_to_teardown_sync (tg: target): unit =
  Target.iter (fun p ->
    Resources.ensure_computed_at p;
    Target.apply_at_path (Barrier_trm.magic_barrier_to_seq kernel_teardown_sync is_smem_or_gmem) p)
  tg
