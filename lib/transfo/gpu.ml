open Prelude
open Target
open Flags

include Gpu_basic

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
