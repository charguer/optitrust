open Ast

(* OpenMP directives *)

let atomic_aux (ao : atomic_operation option) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Atomic ao)] @ lback)
  | _ -> fail t.loc "atomic_aux: expected the sequence where the directive is going to be added"

let atomic (ao : atomic_operation option) (index : int) : Target.Transfo.local =
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

let critical_aux (v : var) (hint : var) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Critical (v, hint))] @ lback)
  | _ -> fail t.loc "critical_aux: expected the sequence where the directive is going to be added"

let critical (v : var) (hint : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (critical_aux v hint index)

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

let declare_target_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Declare_target cl)] @ lback)
  | _ -> fail t.loc "declare_target_aux: expected the sequence where the directive is going to be added"

let declare_target (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (declare_target_aux cl index)

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

let ordered_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Ordered cl)] @ lback)
  | _ -> fail t.loc "ordered_aux: expected the sequence where the directive is going to be added"

let ordered (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (ordered_aux cl index)

let parallel_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Parallel cl)] @ lback)
  | _ -> fail t.loc "parallel_aux: expected the sequence where the directive is going to be added"

let parallel (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (parallel_aux cl index)

let parallel_for_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Parallel_for cl)] @ lback)
  | _ -> fail t.loc "parallel_for_aux: expected the sequence where the directive is going to be added"

let parallel_for (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (parallel_for_aux cl index)

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

let taskloop_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Taskloop cl)] @ lback)
  | _ -> fail t.loc "taskloop_aux: expected the sequence where the directive is going to be added"

let taskloop (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (taskloop_aux cl index)

let taskloop_simd_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Taskloop_simd cl)] @ lback)
  | _ -> fail t.loc "taskloop_simd_aux: expected the sequence where the directive is going to be added"

let taskloop_simd (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (taskloop_simd_aux cl index)

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
  Target.apply_on_path (taskloop_aux cl index)

let teams_distribute_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Teams_distribute cl)] @ lback)
  | _ -> fail t.loc "teams_distribute_aux: expected the sequence where the directive is going to be added"

let teams_distribute (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (taskloop_aux cl index)

let teams_distribute_end_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Teams_distribute_end cl)] @ lback)
  | _ -> fail t.loc "teams_distribute_end_aux: expected the sequence where the directive is going to be added"

let teams_distribute_end (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (taskloop_aux cl index)

let teams_distribute_parallel_for_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Teams_distribute_parallel_for cl)] @ lback)
  | _ -> fail t.loc "teams_distribute_parallel_for_aux: expected the sequence where the directive is going to be added"

let teams_distribute_parallel_for (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (taskloop_aux cl index)

let teams_distribute_parallel_for_simd_aux (cl : clause list) (index : int) (t : trm) : trm =   
  match t.desc with 
  | Trm_seq tl->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Teams_distribute_parallel_for_simd cl)] @ lback)
  | _ -> fail t.loc "teams_distribute_parallel_for_simd_aux: expected the sequence where the directive is going to be added"

let teams_distribute_parallel_for_simd (cl : clause list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (taskloop_aux cl index)


let threadprivate_aux (vl : var list) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_directive (Threadprivate vl)] @ lback)
  | _ -> fail t.loc "threadprivate_aux: expected the sequence where the directive is going to be added"

let threadprivate (vl : var list) (index : int) : Target.Transfo.local = 
  Target.apply_on_path (threadprivate_aux vl index)

(* OpenMP routines *)
let set_num_threads_aux (nb_threads : int) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Set_num_threads nb_threads)] @ lback)

  | _ -> fail t.loc "set_num_threads_aux: expected the sequence where the call to the routine is going to be added"


let set_num_threads (nb_threads : int) (index : int) : Target.Transfo.local = 
  Target.apply_on_path (set_num_threads_aux nb_threads index)


let get_num_threads_aux (nb_threads : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl nb_threads t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var nb_threads) (trm_omp_routine (Get_num_threads))
    | None ->  
      trm_let Var_mutable (nb_threads, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_num_threads)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_num_threads_aux: expected the sequence where the call to the routine is going to be added"

let get_num_threads (nb_threads : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_num_threads_aux nb_threads index)

let get_max_threads_aux (max_threads : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl max_threads t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var max_threads) (trm_omp_routine (Get_max_threads))
    | None ->  
      trm_let Var_mutable (max_threads, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_max_threads)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_max_threads_aux: expected the sequence where the call to the routine is going to be added"

let get_max_threads (max_threads : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_max_threads_aux max_threads index)


let get_thread_num_aux (thread_num : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl thread_num (Trace.get_ast()) in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var thread_num) (trm_omp_routine (Get_thread_num))
    | None ->  
      trm_let Var_mutable (thread_num, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_thread_num)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_thread_num_aux: expected the sequence where the call to the routine is going to be added"

let get_thread_num (thread_num : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_thread_num_aux thread_num index)

let get_num_procs_aux (num_procs : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl num_procs (Trace.get_ast()) in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var num_procs) (trm_omp_routine (Get_num_procs))
    | None ->  
      trm_let Var_mutable (num_procs, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_num_procs)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_num_procs_aux: expected the sequence where the call to the routine is going to be added"

let get_num_procs (num_procs : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_num_procs_aux num_procs index)


let in_parallel_aux (in_parallel : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl in_parallel t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var in_parallel) (trm_omp_routine (In_parallel))
    | None ->  
      trm_let Var_mutable (in_parallel, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (In_parallel)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "in_parallel_aux: expected the sequence where the call to the routine is going to be added"

let in_parallel (in_parallel : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (in_parallel_aux in_parallel index)


let set_dynamic_aux (thread_id : int) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Set_dynamic thread_id)] @ lback)

  | _ -> fail t.loc "set_dynamic_aux: expected the sequence where the call to the routine is going to be added"

let set_dynamic (thread_id : int) (index : int) : Target.Transfo.local = 
  Target.apply_on_path (set_dynamic_aux thread_id index)


let get_dynamic_aux (is_dynamic : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl is_dynamic t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var is_dynamic) (trm_omp_routine (Get_dynamic))
    | None ->  
      trm_let Var_mutable (is_dynamic, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_dynamic)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_dynamic_aux: expected the sequence where the call to the routine is going to be added"

let get_dynamic (is_dynamic : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_dynamic_aux is_dynamic index)


let get_cancellation_aux (is_cancellation : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl is_cancellation t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var is_cancellation) (trm_omp_routine (Get_cancellation))
    | None ->  
      trm_let Var_mutable (is_cancellation, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_cancellation)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_cancellation_aux: expected the sequence where the call to the routine is going to be added"

let get_cancellation (is_cancellation : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_cancellation_aux is_cancellation index)

let set_nested_aux (nested : int) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Set_nested nested)] @ lback)

  | _ -> fail t.loc "set_nested_aux: expected the sequence where the call to the routine is going to be added"

let set_nested (nested : int) (index : int) : Target.Transfo.local = 
  Target.apply_on_path (set_nested_aux nested index)

let get_nested_aux (is_nested : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl is_nested t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var is_nested) (trm_omp_routine (Get_nested))
    | None ->  
      trm_let Var_mutable (is_nested, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_nested)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_nested_aux: expected the sequence where the call to the routine is going to be added"

let get_nested (is_nested : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_nested_aux is_nested index)


let set_schedule_aux (sched_kind : sched_type) (modifier : int) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Set_schedule (sched_kind, modifier))] @ lback)

  | _ -> fail t.loc "set_nested_aux: expected the sequence where the call to the routine is going to be added"

let set_schedule (sched_kind : sched_type) (modifier : int) (index : int) : Target.Transfo.local = 
  Target.apply_on_path (set_schedule_aux sched_kind modifier index)


let get_schedule_aux (sched_kind : sched_type) (modifier : int) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Get_schedule (sched_kind, modifier))] @ lback)

  | _ -> fail t.loc "set_nested_aux: expected the sequence where the call to the routine is going to be added"

let get_schedule (sched_kind : sched_type) (modifier : int) (index : int) : Target.Transfo.local = 
  Target.apply_on_path (get_schedule_aux sched_kind modifier index)

let get_thread_limit_aux (limit : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl limit t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var limit) (trm_omp_routine (Get_thread_limit))
    | None ->  
      trm_let Var_mutable (limit, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_thread_limit)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_thread_limit_aux: expected the sequence where the call to the routine is going to be added"

let get_thread_limit (limit : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_thread_limit_aux limit index)

let set_max_active_levels_aux (max_levels : int) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Set_max_active_levels max_levels)] @ lback)

  | _ -> fail t.loc "set_max_active_levels_aux: expected the sequence where the call to the routine is going to be added"

let set_max_active_levels (max_levels : int) (index : int) : Target.Transfo.local = 
  Target.apply_on_path (set_max_active_levels_aux max_levels index)


let get_max_active_levels_aux (max_levels : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl max_levels t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var max_levels) (trm_omp_routine (Get_max_active_levels))
    | None ->  
      trm_let Var_mutable (max_levels, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_max_active_levels)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_max_active_levels_aux: expected the sequence where the call to the routine is going to be added"

let get_max_active_levels (max_levels : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_max_active_levels_aux max_levels index)

let get_level_aux (level : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl level t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var level) (trm_omp_routine (Get_level))
    | None ->  
      trm_let Var_mutable (level, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_level)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_level_aux: expected the sequence where the call to the routine is going to be added"

let get_level (level : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_level_aux level index)

let get_ancestor_thread_num_aux (thread_num : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl thread_num t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var thread_num) (trm_omp_routine (Get_ancestor_thread_num))
    | None ->  
      trm_let Var_mutable (thread_num, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_ancestor_thread_num)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_ancestor_thread_num_aux: expected the sequence where the call to the routine is going to be added"

let get_ancestor_thread_num (thread_num : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_ancestor_thread_num_aux thread_num index)

let get_team_size_aux (level : int) (size : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl size t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var size) (trm_omp_routine (Get_team_size level))
    | None ->  
      trm_let Var_mutable (size, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_team_size level)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_team_size_aux: expected the sequence where the call to the routine is going to be added"

let get_team_size (level : int) (size : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_team_size_aux level size index)

let get_active_level_aux (active_level : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl active_level t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var active_level) (trm_omp_routine (Get_active_level))
    | None ->  
      trm_let Var_mutable (active_level, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_active_level)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_active_level_aux: expected the sequence where the call to the routine is going to be added"

let get_active_level (active_level : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_active_level_aux active_level index)

let in_final_aux (in_final : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl in_final t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var in_final) (trm_omp_routine (In_final))
    | None ->  
      trm_let Var_mutable (in_final, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (In_final)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "in_final_aux: expected the sequence where the call to the routine is going to be added"

let in_final (in_final : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (in_final_aux in_final index)

let get_proc_bind_aux (proc_bind : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl proc_bind t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var proc_bind) (trm_omp_routine (Get_proc_bind))
    | None ->  
      trm_let Var_mutable (proc_bind, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_proc_bind)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_proc_bind_aux: expected the sequence where the call to the routine is going to be added"

let get_proc_bind (proc_bind : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_proc_bind_aux proc_bind index)

let set_default_device_aux (device_num : var) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Set_default_device device_num)] @ lback)

  | _ -> fail t.loc "set_default_device_aux: expected the sequence where the call to the routine is going to be added"

let set_default_device (device_num : var) (index : int) : Target.Transfo.local = 
  Target.apply_on_path (set_default_device_aux device_num index)

let get_default_device_aux (default_device : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl default_device t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var default_device) (trm_omp_routine (Get_default_device))
    | None ->  
      trm_let Var_mutable (default_device, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_default_device)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_default_device_aux: expected the sequence where the call to the routine is going to be added"

let get_default_device (default_device : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_default_device_aux default_device index)


let get_num_devices_aux (num_devices : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl num_devices t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var num_devices) (trm_omp_routine (Get_num_devices))
    | None ->  
      trm_let Var_mutable (num_devices, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_num_devices)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_num_devices_aux: expected the sequence where the call to the routine is going to be added"

let get_num_devices (num_devices : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_num_devices_aux num_devices index)

let get_num_teams_aux (num_teams : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl num_teams t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var num_teams) (trm_omp_routine (Get_num_teams))
    | None ->  
      trm_let Var_mutable (num_teams, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_num_teams)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_num_teams_aux: expected the sequence where the call to the routine is going to be added"

let get_num_teams (num_teams : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_num_teams_aux num_teams index)

let get_team_num_aux (team_num : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl team_num t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var team_num) (trm_omp_routine (Get_team_num))
    | None ->  
      trm_let Var_mutable (team_num, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (Get_team_num)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_team_num_aux: expected the sequence where the call to the routine is going to be added"

let get_team_num (team_num : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_team_num_aux team_num index)

let is_initial_device_aux (is_initial_device : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl is_initial_device t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var is_initial_device) (trm_omp_routine (In_final))
    | None ->  
      trm_let Var_mutable (is_initial_device, typ_ptr Ptr_kind_mut (typ_int())) (trm_apps (trm_prim(Prim_new (typ_int()))) [trm_omp_routine (In_final)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "is_initial_device_aux: expected the sequence where the call to the routine is going to be added"

let is_initial_device (is_initial_device : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (is_initial_device_aux is_initial_device index)

let init_lock_aux (lock : var) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Init_lock lock)] @ lback)  
  | _ -> fail t.loc "init_lock_aux: expected the sequence where the call to the routine is going to be added"

let init_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (init_lock_aux lock index)

let init_nest_lock_aux (lock : var) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Init_nest_lock lock)] @ lback)  
  | _ -> fail t.loc "init_nest_lock_aux: expected the sequence where the call to the routine is going to be added"

let init_nest_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (init_nest_lock_aux lock index)


let destroy_lock_aux (lock : var) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Destroy_lock lock)] @ lback)  
  | _ -> fail t.loc "destroy_lock_aux: expected the sequence where the call to the routine is going to be added"

let destroy_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (destroy_lock_aux lock index)

let destroy_nest_lock_aux (lock : var) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Destroy_nest_lock lock)] @ lback)  
  | _ -> fail t.loc "destroy_nest_lock_aux: expected the sequence where the call to the routine is going to be added"

let destroy_nest_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (destroy_nest_lock_aux lock index)


let set_lock_aux (lock : var) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Set_lock lock)] @ lback)  
  | _ -> fail t.loc "set_lock_aux: expected the sequence where the call to the routine is going to be added"

let set_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (set_lock_aux lock index)

let set_nest_lock_aux (lock : var) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Set_nest_lock lock)] @ lback)  
  | _ -> fail t.loc "set_nest_lock_aux: expected the sequence where the call to the routine is going to be added"

let set_nest_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (set_nest_lock_aux lock index)


let unset_lock_aux (lock : var) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Unset_lock lock)] @ lback)  
  | _ -> fail t.loc "unset_lock_aux: expected the sequence where the call to the routine is going to be added"

let unset_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (unset_lock_aux lock index)

let unset_nest_lock_aux (lock : var) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Unset_nest_lock lock)] @ lback)  
  | _ -> fail t.loc "unset_nest_lock_aux: expected the sequence where the call to the routine is going to be added"

let unset_nest_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (unset_nest_lock_aux lock index)

let test_lock_aux (lock : var) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Set_lock lock)] @ lback)  
  | _ -> fail t.loc "test_lock_aux: expected the sequence where the call to the routine is going to be added"

let test_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (test_lock_aux lock index)

let test_nest_lock_aux (lock : var) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    trm_seq ~annot:t.annot (lfront @ [trm_omp_routine (Set_nest_lock lock)] @ lback)  
  | _ -> fail t.loc "test_nest_lock_aux: expected the sequence where the call to the routine is going to be added"

let test_nest_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (test_nest_lock_aux lock index)

let get_wtime_aux (wtime : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl wtime t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var wtime) (trm_omp_routine (Get_wtime))
    | None ->  
      trm_let Var_mutable (wtime, typ_ptr Ptr_kind_mut (typ_double())) (trm_apps (trm_prim(Prim_new (typ_double()))) [trm_omp_routine (Get_wtime)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_wtime_aux: expected the sequence where the call to the routine is going to be added"

let get_wtime (wtime : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_wtime_aux wtime index)

let get_wtick_aux (wtick : var) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let find_prev_decl = Internal.toplevel_decl wtick t in
    let new_trm = 
    begin match find_prev_decl with 
    | Some _ -> 
      trm_set (trm_var wtick) (trm_omp_routine (Get_wtick))
    | None ->  
      trm_let Var_mutable (wtick, typ_ptr Ptr_kind_mut (typ_double())) (trm_apps (trm_prim(Prim_new (typ_double()))) [trm_omp_routine (Get_wtick)])
    end in
    trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
  | _ -> fail t.loc "get_wtick_aux: expected the sequence where the call to the routine is going to be added"

let get_wtick (wtick : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_wtick_aux wtick index)
