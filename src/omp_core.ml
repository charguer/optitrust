open Ast

(* OpenMP directives *)

let atomic_aux (ao : atomic_operation) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Atomic ao)] @ lback)
  | _ -> fail t.loc "atomic_aux: expected the sequence where the directive is going to be added"

let atomic (ao : atomic_operation) (index : int) : Target.Transfo.local =
  Target.apply_on_path (atomic_aux ao index)

let atomic_capture_aux (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive Atomic_capture] @ lback)
  | _ -> fail t.loc "atomic_capture_aux: expected the sequence where the directive is going to be added"

let atomic_capture (index : int) : Target.Transfo.local =
  Target.apply_on_path (atomic_capture_aux index)

let barrier_aux (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive Barrier] @ lback)
  | _ -> fail t.loc "barrier_aux: expected the sequence where the directive is going to be added"

let barrier (index : int) : Target.Transfo.local =
  Target.apply_on_path (barrier_aux index)

let cancel_aux (ctc : clause) (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Cancel (ctc, cl))] @ lback)
  | _ -> fail t.loc "cancel_aux: expected the sequence where the directive is going to be added"

let cancel (construct_type_clause : clause) (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (cancel_aux construct_type_clause cl index)

let cancellation_point_aux (ctc : clause) (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Cancellation_point (ctc, cl))] @ lback)
  | _ -> fail t.loc "cancellation_point_aux: expected the sequence where the directive is going to be added"

let cancellation_point (construct_type_clause : clause) (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (cancellation_point_aux construct_type_clause cl index)

let critical_aux (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive Critical] @ lback)
  | _ -> fail t.loc "critical_aux: expected the sequence where the directive is going to be added"

let critical (index : int) : Target.Transfo.local =
  Target.apply_on_path (critical_aux index)

let declare_simd_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Declare_simd cl)] @ lback)
  | _ -> fail t.loc "declare_simd_aux: expected the sequence where the directive is going to be added"

let declare_simd (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (declare_simd_aux cl index)

let declare_reduction_aux (ri : reduction_identifier) (tv : typvar list) (e : expression) (c : clause) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Declare_reduction (ri, tv, e, c))] @ lback)
  | _ -> fail t.loc "declare_reduction_aux: expected the sequence where the directive is going to be added"

let declare_reduction (ri : reduction_identifier) (tv : typvar list) (e : expression) (c : clause) (index : int) : Target.Transfo.local =
  Target.apply_on_path (declare_reduction_aux ri tv e c index)

let declare_target_aux (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive Declare_target] @ lback)
  | _ -> fail t.loc "declare_target_aux: expected the sequence where the directive is going to be added"

let declare_target (index : int) : Target.Transfo.local =
  Target.apply_on_path (declare_target_aux index)

let distribute_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Distribute cl)] @ lback)
  | _ -> fail t.loc "distribute_aux: expected the sequence where the directive is going to be added"

let distribute (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (distribute_aux cl index)

let distribute_parallel_for_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Distribute_parallel_for cl)] @ lback)
  | _ -> fail t.loc "distribute_parallel_for_aux: expected the sequence where the directive is going to be added"

let distribute_parallel_for (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (distribute_parallel_for_aux cl index)

let distribute_parallel_for_simd_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Distribute_parallel_for_simd cl)] @ lback)
  | _ -> fail t.loc "distribute_parallel_for_simd_aux: expected the sequence where the directive is going to be added"

let distribute_parallel_for_simd (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (distribute_parallel_for_simd_aux cl index)

let distribute_simd_aux (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive Distribute_simd] @ lback)
  | _ -> fail t.loc "distribute_simd_aux: expected the sequence where the directive is going to be added"

let distribute_simd (index : int) : Target.Transfo.local =
  Target.apply_on_path (distribute_simd_aux index)

let end_declare_target_aux (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive End_declare_target] @ lback)
  | _ -> fail t.loc "end_declare_target_aux: expected the sequence where the directive is going to be added"

let end_declare_target (index : int) : Target.Transfo.local =
  Target.apply_on_path (end_declare_target_aux index)

let flush_aux (vl : var list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Flush vl)] @ lback)
  | _ -> fail t.loc "flush_aux: expected the sequence where the directive is going to be added"

let flush (vl : var list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (flush_aux vl index)

let for_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (For cl)] @ lback)
  | _ -> fail t.loc "for_aux: expected the sequence where the directive is going to be added"

let for_ (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (for_aux cl index)

let for_simd_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (For_simd cl)] @ lback)
  | _ -> fail t.loc "for_simd_aux: expected the sequence where the directive is going to be added"

let for_simd (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (for_simd_aux cl index)

let master_aux (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive Master] @ lback)
  | _ -> fail t.loc "master_aux: expected the sequence where the directive is going to be added"

let master (index : int) : Target.Transfo.local =
  Target.apply_on_path (master_aux index)

let ordered_aux (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive Ordered] @ lback)
  | _ -> fail t.loc "ordered_aux: expected the sequence where the directive is going to be added"

let ordered (index : int) : Target.Transfo.local =
  Target.apply_on_path (ordered_aux index)

let parallel_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Parallel cl)] @ lback)
  | _ -> fail t.loc "parallel_aux: expected the sequence where the directive is going to be added"

let parallel (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (parallel_aux cl index)

let parallel_for_aux (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive Parallel_for] @ lback)
  | _ -> fail t.loc "parallel_for_aux: expected the sequence where the directive is going to be added"

let parallel_for (index : int) : Target.Transfo.local =
  Target.apply_on_path (parallel_for_aux index)

let parallel_for_simd_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Parallel_for_simd cl)] @ lback)
  | _ -> fail t.loc "parallel_for_simd_aux: expected the sequence where the directive is going to be added"

let parallel_for_simd (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (parallel_for_simd_aux cl index)

let parallel_sections_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Parallel_sections cl)] @ lback)
  | _ -> fail t.loc "parallel_sections_aux: expected the sequence where the directive is going to be added"

let parallel_sections (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (parallel_sections_aux cl index)

let section_aux (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive Section] @ lback)
  | _ -> fail t.loc "section_aux: expected the sequence where the directive is going to be added"

let section (index : int) : Target.Transfo.local =
  Target.apply_on_path (section_aux index)

let sections_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Sections cl)] @ lback)
  | _ -> fail t.loc "sections_aux: expected the sequence where the directive is going to be added"

let sections (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (sections_aux cl index)

let simd_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Simd cl)] @ lback)
  | _ -> fail t.loc "simd_aux: expected the sequence where the directive is going to be added"

let simd (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (simd_aux cl index)

let single_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Single cl)] @ lback)
  | _ -> fail t.loc "single_aux: expected the sequence where the directive is going to be added"

let single (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (single_aux cl index)

let target_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Target cl)] @ lback)
  | _ -> fail t.loc "target_aux: expected the sequence where the directive is going to be added"

let target (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (target_aux cl index)

let target_data_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Target_data cl)] @ lback)
  | _ -> fail t.loc "target_data_aux: expected the sequence where the directive is going to be added"

let target_data (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (target_data_aux cl index)


let target_enter_data_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Target_enter_data cl)] @ lback)
  | _ -> fail t.loc "target_enter_data_aux: expected the sequence where the directive is going to be added"

let target_enter_data (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (target_enter_data_aux cl index)

let target_exit_data_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Target_exit_data cl)] @ lback)
  | _ -> fail t.loc "target_exit_data_aux: expected the sequence where the directive is going to be added"

let target_exit_data (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (target_exit_data_aux cl index)

let target_teams_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Target_teams cl)] @ lback)
  | _ -> fail t.loc "target_teams_aux: expected the sequence where the directive is going to be added"

let target_teams (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (target_teams_aux cl index)

let target_teams_distribute_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Target_teams_distribute cl)] @ lback)
  | _ -> fail t.loc "target_teams_distribute_aux: expected the sequence where the directive is going to be added"

let target_teams_distribute (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (target_teams_distribute_aux cl index)

let target_teams_distribute_parallel_for_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Target_teams_distribute_parallel_for cl)] @ lback)
  | _ -> fail t.loc "target_teams_distribute_parallel_for_aux: expected the sequence where the directive is going to be added"

let target_teams_distribute_parallel_for (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (target_teams_distribute_parallel_for_aux cl index)

let target_teams_distribute_parallel_for_simd_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Target_teams_distribute_parallel_for_simd cl)] @ lback)
  | _ -> fail t.loc "target_teams_distribute_parallel_for_simd_aux: expected the sequence where the directive is going to be added"

let target_teams_distribute_parallel_for_simd (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (target_teams_distribute_parallel_for_simd_aux cl index)

let target_teams_distribute_simd_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Target_teams_distribute_simd cl)] @ lback)
  | _ -> fail t.loc "target_teams_distribute_simd_aux: expected the sequence where the directive is going to be added"

let target_teams_distribute_simd (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (target_teams_distribute_simd_aux cl index)

let target_update_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Target_update cl)] @ lback)
  | _ -> fail t.loc "target_update_aux: expected the sequence where the directive is going to be added"

let target_update (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (target_update_aux cl index)

let task_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Task cl)] @ lback)
  | _ -> fail t.loc "task_aux: expected the sequence where the directive is going to be added"

let task (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (task_aux cl index)

let taskgroup_aux (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive Taskgroup] @ lback)
  | _ -> fail t.loc "taskgroup_aux: expected the sequence where the directive is going to be added"

let taskgroup (index : int) : Target.Transfo.local =
  Target.apply_on_path (taskgroup_aux index)

let task_loop_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Taskloop cl)] @ lback)
  | _ -> fail t.loc "task_loop_aux: expected the sequence where the directive is going to be added"

let task_loop (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (task_loop_aux cl index)

let task_loop_simd_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Taskloop_simd cl)] @ lback)
  | _ -> fail t.loc "task_loop_simd_aux: expected the sequence where the directive is going to be added"

let task_loop_simd (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (task_loop_simd_aux cl index)

let taskwait_aux (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive Taskwait] @ lback)
  | _ -> fail t.loc "taskwait_aux: expected the sequence where the directive is going to be added"

let taskwait (index : int) : Target.Transfo.local =
  Target.apply_on_path (taskwait_aux index)

let taskyield_aux (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive Taskyield] @ lback)
  | _ -> fail t.loc "taskyield_aux: expected the sequence where the directive is going to be added"

let taskyield (index : int) : Target.Transfo.local =
  Target.apply_on_path (taskyield_aux index)

let teams_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Teams cl)] @ lback)
  | _ -> fail t.loc "teams_aux: expected the sequence where the directive is going to be added"

let teams (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (task_loop_aux cl index)

let teams_distribute_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Teams_distribute cl)] @ lback)
  | _ -> fail t.loc "teams_distribute_aux: expected the sequence where the directive is going to be added"

let teams_distribute (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (task_loop_aux cl index)

let teams_distribute_end_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Teams_distribute_end cl)] @ lback)
  | _ -> fail t.loc "teams_distribute_end_aux: expected the sequence where the directive is going to be added"

let teams_distribute_end (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (task_loop_aux cl index)

let teams_distribute_parallel_for_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Teams_distribute_parallel_for cl)] @ lback)
  | _ -> fail t.loc "teams_distribute_parallel_for_aux: expected the sequence where the directive is going to be added"

let teams_distribute_parallel_for (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (task_loop_aux cl index)

let teams_distribute_parallel_for_simd_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Teams_distribute_parallel_for_simd cl)] @ lback)
  | _ -> fail t.loc "teams_distribute_parallel_for_simd_aux: expected the sequence where the directive is going to be added"

let teams_distribute_parallel_for_simd (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (task_loop_aux cl index)


let threadprivate_aux (vl : var list) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Threadprivate vl)] @ lback)
  | _ -> fail t.loc "threadprivate_aux: expected the sequence where the directive is going to be added"

let threadprivate (vl : var list) (index : int) : Target.Transfo.local = 
  Target.apply_on_path (threadprivate_aux vl index)

(* OpenMP routines *)