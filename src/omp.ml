open Ast

(* OpenMP directives *)
let atomic (ao : atomic_operation option) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.atomic ao i t p)

let atomic_capture : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.atomic_capture i t p)

let barrier : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.barrier i t p)

let cancel (construct_type_clause : clause) (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.cancel construct_type_clause cl i t p)

let cancelation_point (construct_type_clause : clause) (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.cancellation_point construct_type_clause cl i t p)

let critical ?(hint : var = "") (v : var) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.critical v hint i t p)

let declare_simd (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.declare_simd cl i t p)

let declare_reduction (ri : reduction_identifier) (tl : typvar list) (e : expression) (cl : clause) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.declare_reduction ri tl e cl i t p)

let declare_target : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.declare_target i t p)

let distribute (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.distribute cl i t p)
  
let distribute_parallel_for (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.distribute_parallel_for cl i t p)

let distribute_parallel_for_simd (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.distribute_parallel_for_simd cl i t p)

let distribute_simd : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.distribute_simd i t p)

let end_declare_target : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.end_declare_target i t p)

let flush (vl : var list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.flush vl i t p)

(* We use here for_ instead of for because for is a keyword in Ocaml *)
let for_ (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.for_ cl i t p)

let for_simd (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.for_simd cl i t p)

let master : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.master i t p)

let ordered : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.ordered i t p)

let parallel (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.parallel cl i t p)

let parallel_for (cl : clause list): Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.parallel_for cl i t p)

let parallel_for_simd (cl : clause list): Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.parallel_for_simd cl i t p)

let parallel_sections (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.parallel_sections cl i t p)

let section : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.section i t p)

let sections (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.sections cl i t p)

let simd (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.simd cl i t p)

let single (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.single cl i t p)

let target (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.target cl i t p)

let target_data (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.target_data cl i t p)

let target_enter_data (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.target_enter_data cl i t p)

let target_exit_data (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.target_exit_data cl i t p)

let target_teams (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.target_teams cl i t p)

let target_teams_distribute (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.target_teams_distribute cl i t p)

let target_teams_distribute_parallel_for (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.target_teams_distribute_parallel_for cl i t p)

let target_teams_distribute_parallel_for_simd (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.target_teams_distribute_parallel_for_simd cl i t p)

let target_teams_distribute_simd (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.target_teams_distribute_simd cl i t p)

let target_update (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.target_update cl i t p)

let task (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.task cl i t p)

let taskgroup : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.taskgroup i t p)

let taskloop (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.taskloop cl i t p)

let taskloop_simd (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.taskloop_simd cl i t p)    

let taskwait : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.taskwait i t p)

let taskyield : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.taskyield i t p)

let teams (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.teams cl i t p)    

let teams_distribute (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.teams_distribute cl i t p)    

let teams_distribute_end (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.teams_distribute_end cl i t p)    

let teams_distribute_parallel_for (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.teams_distribute_parallel_for cl i t p)    

let teams_distribute_parallel_for_simd (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.teams_distribute_parallel_for_simd cl i t p)    

let threadprivate (vl : var list) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.threadprivate vl i t p)


(* OpenMP routines *)
let set_num_threads (nb_threads : int) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.set_num_threads nb_threads i t p)

let get_num_threads (nb_threads : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_num_threads nb_threads i t p)

let get_max_threads (max_threads : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_max_threads max_threads i t p)

let get_thread_num (thread_id : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_thread_num thread_id i t p)

let get_num_procs (num_procs : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_num_procs num_procs i t p)

let in_parallel (in_parallel : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.in_parallel in_parallel i t p)

let set_dynamic (thread_id : int) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.set_dynamic thread_id i t p)

let get_dynamic (is_dynamic : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_dynamic is_dynamic i t p)

let get_cancellation (is_cancellation : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_cancellation is_cancellation i t p)

let set_nested (nested : int) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.set_nested nested i t p)

let get_nested (is_nested : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_nested is_nested i t p)

let set_schedule (sched_kind : sched_type) (modifier : int) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.set_schedule sched_kind modifier i t p)

let get_schedule (sched_kind : sched_type) (modifier : int) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_schedule sched_kind modifier i t p)

let get_thread_limit (limit : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_thread_limit limit i t p)

let set_max_active_levels (max_levels : int) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.set_max_active_levels max_levels i t p)

let get_max_active_levels (max_levels : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_max_active_levels max_levels i t p)

let get_level (level : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_level level i t p)

let get_ancestor_thread_num (thread_num : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) -> 
    Omp_core.get_ancestor_thread_num thread_num i t p)

let get_team_size (level : int) (size : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_team_size level size i t p)

let get_active_level (active_level : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_active_level active_level i t p)

let in_final (in_final : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.in_final in_final i t p)

let set_default_device (device_num : int) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.set_default_device device_num i t p)

let get_default_device (default_device : var) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_default_device default_device i t p)

let get_proc_bind (proc_bind : var) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_proc_bind  proc_bind i t p)

let get_num_devices (num_devices : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_num_devices num_devices i t p)

let get_num_teams (num_teams : var) : Target.Transfo.t =
  Target.apply_on_target_between (fun t (p, i) ->
    Omp_core.get_num_teams num_teams i t p)

let get_team_num (team_num : var) : Target.Transfo.t =
  Target.apply_on_target_between (fun t (p, i) ->
    Omp_core.get_team_num team_num i t p)

let is_initial_device (is_initial_device : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.is_initial_device is_initial_device i t p)

let init_lock (lock : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.init_lock lock i t p)

let init_nest_lock (lock : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.init_nest_lock lock i t p)


let set_lock (lock : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.set_lock lock i t p)

let set_nest_lock (lock : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.set_nest_lock lock i t p)

let unset_lock (lock : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.unset_lock lock i t p)

let unset_nest_lock (lock : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.unset_nest_lock lock i t p)

let test_lock (lock : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.test_lock lock i t p)

let test_nest_lock (lock : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.test_nest_lock lock i t p)

let get_wtime (wtime : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_wtime wtime i t p)

let get_wtick (wtick : var) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.get_wtick wtick i t p)

