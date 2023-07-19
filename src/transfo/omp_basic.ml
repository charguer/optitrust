open Syntax
open Target


(* Module for generating OpenMP directives and routines. Directives are just annotations inserted to the trm before which
   the directive is going to appear. Routines are trms, so they are inserted as instructions. *)

(******************************************************************************)
(*                            OpenMP directives                               *)
(******************************************************************************)
let%transfo atomic ?(ao : atomic_operation option) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Atomic ao)) tg

let%transfo atomic_capture (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Atomic_capture)) tg

let%transfo barrier (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Barrier)) tg

let%transfo cancel ?(clause : clause list = []) (construct_type_clause : clause) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Cancel (construct_type_clause, clause))) tg

let%transfo cancellation_point ?(clause : clause list = []) (construct_type_clause : clause) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Cancellation_point (construct_type_clause, clause))) tg

let%transfo critical ?(hint : var = "") (v : var) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Critical (v, hint))) tg

let%transfo declare_simd ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Declare_simd clause )) tg

let%transfo declare_reduction (ri : reduction_identifier) (tl : typvars) (e : expression) (clause : clause) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Declare_reduction (ri, tl, e, clause))) tg

let%transfo declare_target ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Declare_target clause)) tg

let%transfo distribute ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Distribute clause)) tg

let%transfo distribute_parallel_for ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Distribute_parallel_for clause )) tg

let%transfo distribute_parallel_for_simd ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Distribute_parallel_for_simd clause )) tg

let%transfo distribute_simd (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Distribute_simd)) tg

let%transfo end_declare_target (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (End_declare_target)) tg

let%transfo flush (vl : vars) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Flush vl)) tg

let%transfo for_ ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (For clause)) tg

let%transfo for_simd ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (For_simd clause)) tg

let%transfo master (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Master)) tg

let%transfo ordered ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Ordered clause)) tg

let%transfo parallel ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Parallel clause)) tg

let%transfo parallel_for ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Parallel_for clause)) tg

let%transfo parallel_for_simd ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Parallel_for_simd clause)) tg

let%transfo parallel_sections ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Parallel_sections clause)) tg

let%transfo section (tg : target) : unit =
  transfo_on_targets (trm_add_pragma Section) tg

let%transfo simd ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Simd clause)) tg

let%transfo single ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Single clause)) tg

let%transfo target ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Target clause)) tg

let%transfo target_data ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Target_data clause)) tg

let%transfo target_enter_data ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Target_enter_data clause)) tg

let%transfo target_exit_data ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Target_exit_data clause)) tg

let%transfo target_teams ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Target_teams clause)) tg

let%transfo target_teams_distribute ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Target_teams_distribute clause)) tg

let%transfo target_teams_distribute_parallel_for ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Target_teams_distribute_parallel_for clause)) tg

let%transfo target_teams_distribute_parallel_for_simd ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Target_teams_distribute_parallel_for_simd clause)) tg

let%transfo target_teams_distribute_simd ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Target_teams_distribute_simd clause)) tg

let%transfo target_update ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Target_update clause)) tg

let%transfo task ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Task clause)) tg

let%transfo taskgroup (tg : target) : unit =
  transfo_on_targets (trm_add_pragma Taskgroup) tg

let%transfo taskloop ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Taskloop clause)) tg

let%transfo taskloop_simd ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Taskloop_simd clause)) tg

let%transfo taskwait ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Taskwait clause)) tg

let%transfo taskyield  (tg : target) : unit =
  transfo_on_targets (trm_add_pragma Taskyield) tg

let%transfo teams ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Teams clause)) tg

let%transfo teams_distribute ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Teams_distribute clause)) tg

let%transfo teams_distribute_end ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Teams_distribute_end clause)) tg

let%transfo teams_distribute_parallel_for ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Teams_distribute_parallel_for clause)) tg

let%transfo teams_distribute_parallel_for_simd ?(clause : clause list = []) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Teams_distribute_parallel_for_simd clause)) tg

let%transfo threadprivate (vl : vars) (tg : target) : unit =
  transfo_on_targets (trm_add_pragma (Threadprivate vl)) tg


(******************************************************************************)
(*                             OpenMP routines                                *)
(******************************************************************************)
let%transfo set_num_threads (nb_threads : int) (tg : target) : unit=
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_num_threads nb_threads i t p) tg

let%transfo get_num_threads (nb_threads : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_num_threads nb_threads i t p) tg

let%transfo declare_num_threads ?(tg : target = [tFirst; dRoot]) (nb_threads : var) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.declare_num_threads nb_threads i t p) tg

let%transfo get_max_threads (max_threads : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_max_threads max_threads i t p) tg

let%transfo get_thread_num ?(const : bool = true) (thread_id : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_thread_num const thread_id i t p) tg

let%transfo get_num_procs (num_procs : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_num_procs num_procs i t p) tg

let%transfo in_parallel (in_parallel : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.in_parallel in_parallel i t p) tg

let%transfo set_dynamic (thread_id : int) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_dynamic thread_id i t p) tg

let%transfo get_dynamic (is_dynamic : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_dynamic is_dynamic i t p) tg

let%transfo get_cancellation (is_cancellation : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_cancellation is_cancellation i t p) tg

let%transfo set_nested (nested : int) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_nested nested i t p) tg

let%transfo get_nested (is_nested : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_nested is_nested i t p) tg

let%transfo set_schedule (sched_kind : sched_type) (modifier : int) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_schedule sched_kind modifier i t p) tg

let%transfo get_schedule (sched_kind : sched_type) (modifier : int) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_schedule sched_kind modifier i t p) tg

let%transfo get_thread_limit (limit : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_thread_limit limit i t p) tg

let%transfo set_max_active_levels (max_levels : int) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_max_active_levels max_levels i t p) tg

let%transfo get_max_active_levels (max_levels : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_max_active_levels max_levels i t p) tg

let%transfo get_level (level : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_level level i t p) tg

let%transfo get_ancestor_thread_num (thread_num : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_ancestor_thread_num thread_num i t p) tg

let%transfo get_team_size (level : int) (size : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_team_size level size i t p) tg

let%transfo get_active_level (active_level : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_active_level active_level i t p) tg

let%transfo in_final (in_final : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.in_final in_final i t p) tg

let%transfo set_default_device (device_num : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_default_device device_num i t p) tg

let%transfo get_default_device (default_device : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_default_device default_device i t p) tg

let%transfo get_proc_bind (proc_bind : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_proc_bind  proc_bind i t p) tg

let%transfo get_num_devices (num_devices : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_num_devices num_devices i t p) tg

let%transfo get_num_teams (num_teams : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_num_teams num_teams i t p) tg

let%transfo get_team_num (team_num : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_team_num team_num i t p) tg

let%transfo is_initial_device (is_initial_device : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.is_initial_device is_initial_device i t p) tg

let%transfo init_lock (lock : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.init_lock lock i t p) tg

let%transfo init_nest_lock (lock : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.init_nest_lock lock i t p) tg

let%transfo destroy_lock (lock : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.destroy_lock lock i t p) tg

let%transfo destroy_nest_lock (lock : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.destroy_nest_lock lock i t p) tg

let%transfo set_lock (lock : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_lock lock i t p) tg

let%transfo set_nest_lock (lock : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_nest_lock lock i t p) tg

let%transfo unset_lock (lock : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.unset_lock lock i t p) tg

let%transfo unset_nest_lock (lock : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.unset_nest_lock lock i t p) tg

let%transfo test_lock (lock : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.test_lock lock i t p) tg

let%transfo test_nest_lock (lock : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.test_nest_lock lock i t p) tg

let%transfo get_wtime (wtime : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_wtime wtime i t p) tg

let%transfo get_wtick (wtick : var) (tg : target) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_wtick wtick i t p) tg
