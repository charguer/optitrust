open Prelude
open Target


(** Module for generating OpenMP directives and routines. Directives are just annotations inserted to the trm before which
   the directive is going to appear. Routines are trms, so they are inserted as instructions. *)

(******************************************************************************)
(*                            OpenMP directives                               *)
(******************************************************************************)
let%transfo atomic ?(ao : atomic_operation option) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Atomic ao)) tg

let%transfo atomic_capture (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Atomic_capture)) tg

let%transfo barrier (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Barrier)) tg

let%transfo cancel ?(clause : clause list = []) (construct_type_clause : clause) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Cancel (construct_type_clause, clause))) tg

let%transfo cancellation_point ?(clause : clause list = []) (construct_type_clause : clause) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Cancellation_point (construct_type_clause, clause))) tg

let%transfo critical ?(hint : string = "") (v : var) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Critical (v, hint))) tg

let%transfo declare_simd ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Declare_simd clause )) tg

let%transfo declare_reduction (ri : reduction_identifier) (tl : typvars) (e : expression) (clause : clause) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Declare_reduction (ri, tl, e, clause))) tg

let%transfo declare_target ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Declare_target clause)) tg

let%transfo distribute ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Distribute clause)) tg

let%transfo distribute_parallel_for ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Distribute_parallel_for clause )) tg

let%transfo distribute_parallel_for_simd ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Distribute_parallel_for_simd clause )) tg

let%transfo distribute_simd (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Distribute_simd)) tg

let%transfo end_declare_target (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (End_declare_target)) tg

let%transfo flush (vl : vars) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Flush vl)) tg

let add_pragma_on_parallelizable_for (directive: directive) (t: trm): trm =
  if !Flags.check_validity then begin
    let error = "OMP transformation is invalid: it is not applied on a for loop." in
    let _, _, contract = trm_inv ~error trm_for_inv t in
    let error = "OMP transformation is invalid" in
    Resources.justif_parallelizable_loop_contract ~error contract;
  end;
  trm_add_pragma directive t

let%transfo for_ ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (add_pragma_on_parallelizable_for (For clause)) tg

let%transfo for_simd ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (add_pragma_on_parallelizable_for (For_simd clause)) tg

let%transfo master (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Master)) tg

let%transfo ordered ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Ordered clause)) tg

let%transfo parallel ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Parallel clause)) tg

let%transfo parallel_for ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (add_pragma_on_parallelizable_for (Parallel_for clause)) tg

let%transfo parallel_for_simd ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (add_pragma_on_parallelizable_for (Parallel_for_simd clause)) tg

let%transfo parallel_sections ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Parallel_sections clause)) tg

let%transfo section (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma Section) tg

let%transfo simd ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (add_pragma_on_parallelizable_for (Simd clause)) tg

let%transfo single ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Single clause)) tg

let%transfo target ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Target clause)) tg

let%transfo target_data ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Target_data clause)) tg

let%transfo target_enter_data ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Target_enter_data clause)) tg

let%transfo target_exit_data ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Target_exit_data clause)) tg

let%transfo target_teams ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Target_teams clause)) tg

let%transfo target_teams_distribute ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Target_teams_distribute clause)) tg

let%transfo target_teams_distribute_parallel_for ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Target_teams_distribute_parallel_for clause)) tg

let%transfo target_teams_distribute_parallel_for_simd ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Target_teams_distribute_parallel_for_simd clause)) tg

let%transfo target_teams_distribute_simd ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Target_teams_distribute_simd clause)) tg

let%transfo target_update ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Target_update clause)) tg

let%transfo task ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Task clause)) tg

let%transfo taskgroup (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma Taskgroup) tg

let%transfo taskloop ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Taskloop clause)) tg

let%transfo taskloop_simd ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Taskloop_simd clause)) tg

let%transfo taskwait ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Taskwait clause)) tg

let%transfo taskyield  (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma Taskyield) tg

let%transfo teams ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Teams clause)) tg

let%transfo teams_distribute ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Teams_distribute clause)) tg

let%transfo teams_distribute_end ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Teams_distribute_end clause)) tg

let%transfo teams_distribute_parallel_for ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Teams_distribute_parallel_for clause)) tg

let%transfo teams_distribute_parallel_for_simd ?(clause : clause list = []) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Teams_distribute_parallel_for_simd clause)) tg

let%transfo threadprivate (vl : vars) (tg : target) : unit =
  apply_at_target_paths (trm_add_pragma (Threadprivate vl)) tg


(******************************************************************************)
(*                             OpenMP routines                                *)
(******************************************************************************)
let%transfo set_num_threads (nb_threads : int) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.set_num_threads_at nb_threads i t) tg

let%transfo get_num_threads (nb_threads : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_num_threads_at nb_threads i t) tg

let%transfo declare_num_threads ?(tg : target = [tFirst; dRoot]) (nb_threads : var) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.declare_num_threads_at nb_threads i t) tg

let%transfo get_max_threads (max_threads : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_max_threads_at max_threads i t) tg

let%transfo get_thread_num ?(const : bool = true) (thread_id : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_thread_num_at const thread_id i t) tg

let%transfo get_num_procs (num_procs : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_num_procs_at num_procs i t) tg

let%transfo in_parallel (in_parallel : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.in_parallel_at in_parallel i t) tg

let%transfo set_dynamic (thread_id : int) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.set_dynamic_at thread_id i t) tg

let%transfo get_dynamic (is_dynamic : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_dynamic_at is_dynamic i t) tg

let%transfo get_cancellation (is_cancellation : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_cancellation_at is_cancellation i t) tg

let%transfo set_nested (nested : int) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.set_nested_at nested i t) tg

let%transfo get_nested (is_nested : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_nested_at is_nested i t) tg

let%transfo set_schedule (sched_kind : sched_type) (modifier : int) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.set_schedule_at sched_kind modifier i t) tg

let%transfo get_schedule (sched_kind : sched_type) (modifier : int) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_schedule_at sched_kind modifier i t) tg

let%transfo get_thread_limit (limit : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_thread_limit_at limit i t) tg

let%transfo set_max_active_levels (max_levels : int) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.set_max_active_levels_at max_levels i t) tg

let%transfo get_max_active_levels (max_levels : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_max_active_levels_at max_levels i t) tg

let%transfo get_level (level : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_level_at level i t) tg

let%transfo get_ancestor_thread_num (thread_num : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_ancestor_thread_num_at thread_num i t) tg

let%transfo get_team_size (level : int) (size : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_team_size_at level size i t) tg

let%transfo get_active_level (active_level : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_active_level_at active_level i t) tg

let%transfo in_final (in_final : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.in_final_at in_final i t) tg

let%transfo set_default_device (device_num : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.set_default_device_at device_num i t) tg

let%transfo get_default_device (default_device : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_default_device_at default_device i t) tg

let%transfo get_proc_bind (proc_bind : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_proc_bind_at proc_bind i t) tg

let%transfo get_num_devices (num_devices : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_num_devices_at num_devices i t) tg

let%transfo get_num_teams (num_teams : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_num_teams_at num_teams i t) tg

let%transfo get_team_num (team_num : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_team_num_at team_num i t) tg

let%transfo is_initial_device (is_initial_device : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.is_initial_device_at is_initial_device i t) tg

let%transfo init_lock (lock : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.init_lock_at lock i t) tg

let%transfo init_nest_lock (lock : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.init_nest_lock_at lock i t) tg

let%transfo destroy_lock (lock : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.destroy_lock_at lock i t) tg

let%transfo destroy_nest_lock (lock : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.destroy_nest_lock_at lock i t) tg

let%transfo set_lock (lock : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.set_lock_at lock i t) tg

let%transfo set_nest_lock (lock : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.set_nest_lock_at lock i t) tg

let%transfo unset_lock (lock : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.unset_lock_at lock i t) tg

let%transfo unset_nest_lock (lock : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.unset_nest_lock_at lock i t) tg

let%transfo test_lock (lock : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.test_lock_at lock i t) tg

let%transfo test_nest_lock (lock : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.test_nest_lock_at lock i t) tg

let%transfo get_wtime (wtime : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_wtime_at wtime i t) tg

let%transfo get_wtick (wtick : var) (tg : target) : unit =
  apply_at_target_paths_before (fun t i ->
    Omp_core.get_wtick_at wtick i t) tg
