open Ast
open Target


(* Module for generating OpenMP directives and routines. Directives are just annotations inserted to the trm before which
   the directive is going to appear. Routines are trms, so they are inserted as instructions. *)

(******************************************************************************)
(*                            OpenMP directives                               *)
(******************************************************************************)
let atomic ?(ao : atomic_operation option = None) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Atomic ao))

let atomic_capture : Transfo.t =
  transfo_on_targets (trm_add_pragma (Atomic_capture))

let barrier : Transfo.t =
  transfo_on_targets (trm_add_pragma (Barrier))

let cancel ?(clause : clause list = []) (construct_type_clause : clause) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Cancel (construct_type_clause, clause)))

let cancellation_point ?(clause : clause list = []) (construct_type_clause : clause) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Cancellation_point (construct_type_clause, clause)))

let critical ?(hint : var = "") (v : var) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Critical (v, hint)))

let declare_simd ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Declare_simd clause ))

let declare_reduction (ri : reduction_identifier) (tl : typvars) (e : expression) (clause : clause) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Declare_reduction (ri, tl, e, clause)))

let declare_target ?(clause : clause list = []): Transfo.t =
  transfo_on_targets (trm_add_pragma (Declare_target clause))

let distribute ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Distribute clause))

let distribute_parallel_for ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Distribute_parallel_for clause ))

let distribute_parallel_for_simd ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Distribute_parallel_for_simd clause ))

let distribute_simd : Transfo.t =
  transfo_on_targets (trm_add_pragma (Distribute_simd))

let end_declare_target  : Transfo.t =
  transfo_on_targets (trm_add_pragma (End_declare_target))

let flush (vl : vars) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Flush vl))

let for_ ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (For clause))

let for_simd ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (For_simd clause))

let master : Transfo.t =
  transfo_on_targets (trm_add_pragma (Master))

let oredered ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Ordered clause))

let parallel ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Parallel clause))

let parallel_for ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Parallel_for clause))

let parallel_for_simd ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Parallel_for_simd clause))

let parallel_sections ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Parallel_sections clause))

let sections ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Sections clause))

let simd ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Simd clause))

let single ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Single clause))

let target ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Target clause))

let target_data ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Target_data clause))

let target_enter_data ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Target_enter_data clause))

let target_exit_data ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Target_exit_data clause))

let target_teams ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Target_teams clause))

let target_teams_distribute ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Target_teams_distribute clause))

let target_teams_distribute_parallel_for ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Target_teams_distribute_parallel_for clause))

let target_teams_distribute_parallel_for_simd ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Target_teams_distribute_parallel_for_simd clause))

let target_teams_distribute_simd ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Target_teams_distribute_simd clause))

let target_update ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Target_update clause))

let task ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Task clause))

let taskgroup : Transfo.t =
  transfo_on_targets (trm_add_pragma Taskgroup)

let taskloop ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Taskloop clause))

let taskloop_simd ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Taskloop_simd clause))

let taskwait ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma Taskwait)

let taskyield  : Transfo.t =
  transfo_on_targets (trm_add_pragma Taskyield)

let teams ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Teams clause))

let teams_distribute ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Teams_distribute clause))

let teams_distribute_end ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Teams_distribute_end clause))

let teams_distribute_parallel_for ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Teams_distribute_parallel_for clause))

let teams_distribute_parallel_for_simd ?(clause : clause list = []) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Teams_distribute_parallel_for_simd clause))

let threadprivate (vl : vars) : Transfo.t =
  transfo_on_targets (trm_add_pragma (Threadprivate vl))


(******************************************************************************)
(*                             OpenMP routines                                *)
(******************************************************************************)
let set_num_threads (nb_threads : int) : Transfo.t=
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_num_threads nb_threads i t p)

let get_num_threads (nb_threads : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_num_threads nb_threads i t p)

let declare_num_threads ?(tg : target = [tFirst; dRoot]) (nb_threads : var) : unit =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.declare_num_threads nb_threads i t p) tg

let get_max_threads (max_threads : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_max_threads max_threads i t p)

let get_thread_num ?(const : bool = true) (thread_id : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_thread_num const thread_id i t p)

let get_num_procs (num_procs : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_num_procs num_procs i t p)

let in_parallel (in_parallel : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.in_parallel in_parallel i t p)

let set_dynamic (thread_id : int) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_dynamic thread_id i t p)

let get_dynamic (is_dynamic : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_dynamic is_dynamic i t p)

let get_cancellation (is_cancellation : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_cancellation is_cancellation i t p)

let set_nested (nested : int) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_nested nested i t p)

let get_nested (is_nested : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_nested is_nested i t p)

let set_schedule (sched_kind : sched_type) (modifier : int) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_schedule sched_kind modifier i t p)

let get_schedule (sched_kind : sched_type) (modifier : int) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_schedule sched_kind modifier i t p)

let get_thread_limit (limit : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_thread_limit limit i t p)

let set_max_active_levels (max_levels : int) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_max_active_levels max_levels i t p)

let get_max_active_levels (max_levels : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_max_active_levels max_levels i t p)

let get_level (level : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_level level i t p)

let get_ancestor_thread_num (thread_num : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_ancestor_thread_num thread_num i t p)

let get_team_size (level : int) (size : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_team_size level size i t p)

let get_active_level (active_level : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_active_level active_level i t p)

let in_final (in_final : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.in_final in_final i t p)

let set_default_device (device_num : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_default_device device_num i t p)

let get_default_device (default_device : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_default_device default_device i t p)

let get_proc_bind (proc_bind : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_proc_bind  proc_bind i t p)

let get_num_devices (num_devices : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_num_devices num_devices i t p)

let get_num_teams (num_teams : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_num_teams num_teams i t p)

let get_team_num (team_num : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_team_num team_num i t p)

let is_initial_device (is_initial_device : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.is_initial_device is_initial_device i t p)

let init_lock (lock : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.init_lock lock i t p)

let init_nest_lock (lock : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.init_nest_lock lock i t p)

let destroy_lock (lock : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.destroy_lock lock i t p)

let destroy_nest_lock (lock : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.destroy_nest_lock lock i t p)

let set_lock (lock : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_lock lock i t p)

let set_nest_lock (lock : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.set_nest_lock lock i t p)

let unset_lock (lock : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.unset_lock lock i t p)

let unset_nest_lock (lock : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.unset_nest_lock lock i t p)

let test_lock (lock : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.test_lock lock i t p)

let test_nest_lock (lock : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.test_nest_lock lock i t p)

let get_wtime (wtime : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_wtime wtime i t p)

let get_wtick (wtick : var) : Transfo.t =
  apply_on_targets_between (fun t (p, i) ->
    Omp_core.get_wtick wtick i t p)
