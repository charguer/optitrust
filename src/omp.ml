open Ast


let atomic (ao : atomic_operation) : Target.Transfo.t = 
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

let critical : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.critical i t p)

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

let parallel_for : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.parallel_for i t p)

let parallel_for_simd : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.parallel_for i t p)

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

let task_loop (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.task_loop cl i t p)

let task_loop_simd (cl : clause list) : Target.Transfo.t = 
  Target.apply_on_target_between(fun t (p, i) ->
    Omp_core.task_loop_simd cl i t p)    

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