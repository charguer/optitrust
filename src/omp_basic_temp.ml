open Ast
open Target

let atomic ?(ao : atomic_operation option = None) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Atomic ao)))

let atomic_capture : Transfo.t =
  apply_on_targets ( apply_on_path (trm_add_pragma (Atomic_capture)))

let barrier : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Barrier)))

let cancel ?(clause : clause list = []) (construct_type_clause : clause) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Cancel (construct_type_clause, clause))))

let cancellation_point ?(clause : clause list = []) (construct_type_clause : clause) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Cancellation_point (construct_type_clause, clause))))

let critical ?(hint : var = "") (v : var) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Critical (v, hint))))
  
let declare_simd ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Declare_simd clause )))
  
let declare_reduction (ri : reduction_identifier) (tl : typvars) (e : expression) (clause : clause) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Declare_reduction (ri, tl, e, clause))))

let declare_target ?(clause : clause list = []): Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Declare_target clause)))

let distribute ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Distribute clause)))

let distribute_parallel_for ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Distribute_parallel_for clause )))

let distribute_parallel_for_simd ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Distribute_parallel_for_simd clause )))

let distribute_simd : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Distribute_simd)))

let end_declare_target  : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (End_declare_target)))

let flush (vl : vars) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Flush vl)))

let for_ ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (For clause)))

let for_simd ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (For_simd clause)))

let master : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Master)))

let oredered ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Ordered clause)))

let parallel ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Parallel clause)))
let parallel_for ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Parallel_for clause)))

let parallel_for_simd ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Parallel_for_simd clause)))

let parallel_sections ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Parallel_sections clause)))

let sections ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Sections clause)))

let simd ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Simd clause)))

let single ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Single clause)))

let target ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Target clause)))

let target_data ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Target_data clause)))

let target_enter_data ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Target_enter_data clause)))

let target_exit_data ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Target_exit_data clause)))

let target_teams ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Target_teams clause)))

let target_teams_distribute ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Target_teams_distribute clause)))

let target_teams_distribute_parallel_for ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Target_teams_distribute_parallel_for clause)))

let target_teams_distribute_parallel_for_simd ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Target_teams_distribute_parallel_for_simd clause)))

let target_teams_distribute_simd ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Target_teams_distribute_simd clause)))

let target_update ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Target_update clause)))

let task ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Task clause)))

let taskgroup : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma Taskgroup))

let taskloop ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Taskloop clause)))

let taskloop_simd ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Taskloop_simd clause)))

let taskwait ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma Taskwait))

let taskyield  : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma Taskyield))

let teams ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Teams clause)))

let teams_distribute ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Teams_distribute clause)))

let teams_distribute_end ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Teams_distribute_end clause)))

let teams_distribute_parallel_for ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Teams_distribute_parallel_for clause)))

let teams_distribute_parallel_for_simd ?(clause : clause list = []) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Teams_distribute_parallel_for_simd clause)))

let threadprivate (vl : vars) : Transfo.t =
  apply_on_targets (apply_on_path (trm_add_pragma (Threadprivate vl)))
